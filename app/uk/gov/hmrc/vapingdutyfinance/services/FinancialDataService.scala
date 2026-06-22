/*
 * Copyright 2025 HM Revenue & Customs
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package uk.gov.hmrc.vapingdutyfinance.services

import play.api.Logging
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.vapingdutyfinance.config.AppConfig
import uk.gov.hmrc.vapingdutyfinance.connectors.FinancialDataConnector
import uk.gov.hmrc.vapingdutyfinance.models.{OutstandingPayment, PaymentStatus}
import uk.gov.hmrc.vapingdutyfinance.models.financialdata.*

import java.time.format.DateTimeFormatter
import java.time.{Clock, LocalDate}
import javax.inject.{Inject, Singleton}
import scala.concurrent.{ExecutionContext, Future}

@Singleton
class FinancialDataService @Inject()(
                                      connector: FinancialDataConnector,
                                      appConfig: AppConfig,
                                      clock: Clock
                                    )(using ExecutionContext) extends Logging {

  private val dateFormatter = DateTimeFormatter.ISO_LOCAL_DATE

  def getOutstandingPayments(
                              vpdId: String,
                              dateFrom: Option[LocalDate],
                              dateTo: Option[LocalDate]
                            )(using HeaderCarrier): Future[Either[String, Seq[OutstandingPayment]]] = {

    // Return static data if flag is enabled
    if (appConfig.useStaticFinancialData) {
      logger.info(s"Using static financial data for vpdId=$vpdId")
      return Future.successful(Right(getStaticOutstandingPayments()))
    }

    val effectiveDateFrom = dateFrom.getOrElse(
      LocalDate.now(clock).minusMonths(appConfig.defaultDateRangeMonths.toLong)
    )
    val effectiveDateTo = dateTo.getOrElse(LocalDate.now(clock))

    val request = buildRequest(vpdId, effectiveDateFrom, effectiveDateTo)

    connector.getFinancialData(request).map {
      case Right(response) =>
        val payments = transformToOutstandingPayments(response)
        Right(payments)
      case Left(error) =>
        val errorMessage = mapErrorCodeToMessage(error.errors.code)
        logger.warn(s"Error from financial data API: code=${error.errors.code}, message=${error.errors.text}")
        Left(errorMessage)
    }.recover { case ex =>
      logger.warn(s"Unexpected error calling financial data API: ${ex.getMessage}", ex)
      Left("An unexpected error occurred while retrieving financial data")
    }
  }

  private def buildRequest(
                            vpdId: String,
                            dateFrom: LocalDate,
                            dateTo: LocalDate
                          ): FinancialDataRequest = {
    FinancialDataRequest(
      taxRegime = appConfig.taxRegimeVpd,
      taxpayerInformation = TaxpayerInformation(
        idType = appConfig.idTypeVpd,
        idNumber = vpdId
      ),
      selectionCriteria = SelectionCriteria(
        dateRange = DateRange(
          dateType = appConfig.dateTypePosting,
          dateFrom = dateFrom,
          dateTo = dateTo
        ),
        includeClearedItems = appConfig.includeClearedItemsDefault,
        includeStatisticalItems = appConfig.includeStatisticalItemsDefault,
        includePaymentOnAccount = appConfig.includePaymentOnAccountDefault
      ),
      dataEnrichment = DataEnrichmentOptions(
        addRegimeTotalisation = appConfig.addRegimeTotalisationDefault,
        addLockInformation = appConfig.addLockInformationDefault,
        addPenaltyDetails = appConfig.addPenaltyDetailsDefault,
        addPostedInterestDetails = appConfig.addPostedInterestDetailsDefault,
        addAccruingInterestDetails = appConfig.addAccruingInterestDetailsDefault
      )
    )
  }

  private def transformToOutstandingPayments(response: FinancialDataResponse): Seq[OutstandingPayment] = {
    response.success.financialData
      .flatMap(_.documentDetails)
      .getOrElse(Seq.empty)
      .filter(doc => doc.documentOutstandingAmount.exists(_ > 0))
      .flatMap { doc =>
        doc.lineItemDetails.getOrElse(Seq.empty).map { lineItem =>
          val period = formatPeriod(lineItem.periodFromDate, lineItem.periodToDate)
          val dueDate = lineItem.netDueDate.map(_.format(dateFormatter)).getOrElse("Unknown")
          val status = determineStatus(lineItem.netDueDate)

          OutstandingPayment(
            chargeReference = doc.chargeReferenceNumber.getOrElse("Unknown"),
            period = period,
            amountDue = doc.documentOutstandingAmount.getOrElse(BigDecimal(0)),
            dueDate = dueDate,
            status = status
          )
        }
      }
  }

  private def formatPeriod(fromDate: Option[LocalDate], toDate: Option[LocalDate]): String = {
    (fromDate, toDate) match {
      case (Some(from), Some(to)) => s"${from.format(dateFormatter)} to ${to.format(dateFormatter)}"
      case (Some(from), None) => s"From ${from.format(dateFormatter)}"
      case (None, Some(to)) => s"To ${to.format(dateFormatter)}"
      case _ => "Unknown period"
    }
  }

  private def determineStatus(netDueDate: Option[LocalDate]): PaymentStatus = {
    netDueDate match {
      case Some(dueDate) if dueDate.isBefore(LocalDate.now(clock)) => PaymentStatus.Overdue
      case Some(_) => PaymentStatus.Due
      case None => PaymentStatus.Due
    }
  }

  private def mapErrorCodeToMessage(code: String): String = code match {
    case "002" => "Invalid tax regime"
    case "003" => "Request could not be processed"
    case "015" => "Invalid ID type"
    case "016" => "Invalid ID number"
    case "017" => "Invalid search type or parameters"
    case "018" => "No data found"
    case "019" => "Invalid date type"
    case "020" => "Invalid date range"
    case "135" => "Duplicate submission reference"
    case _ => "An error occurred while retrieving financial data"
  }

  private def getStaticOutstandingPayments(): Seq[OutstandingPayment] = {
    val payments = Seq(
      OutstandingPayment(
        chargeReference = "XM002610011594",
        period = "2024-01-01 to 2024-01-31",
        amountDue = BigDecimal("1250.50"),
        dueDate = "2024-02-15",
        status = PaymentStatus.Overdue
      ),
      OutstandingPayment(
        chargeReference = "XM002610011595",
        period = "2024-02-01 to 2024-02-29",
        amountDue = BigDecimal("2500.00"),
        dueDate = LocalDate.now(clock).plusDays(5).format(dateFormatter),
        status = PaymentStatus.Due
      ),
      OutstandingPayment(
        chargeReference = "XM002610011596",
        period = "2024-03-01 to 2024-03-31",
        amountDue = BigDecimal("750.25"),
        dueDate = LocalDate.now(clock).plusDays(15).format(dateFormatter),
        status = PaymentStatus.Due
      )
    )
    
    // If all amounts are zero, return a single payment with NothingToPay status
    if (payments.forall(_.amountDue == BigDecimal(0))) {
      Seq(OutstandingPayment(
        chargeReference = "",
        period = "",
        amountDue = BigDecimal(0),
        dueDate = "",
        status = PaymentStatus.NothingToPay
      ))
    } else {
      payments
    }
  }
}
