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
import uk.gov.hmrc.vapingdutyfinance.models.{ClearedPayment, OutstandingPayment, PaymentStatus, PaymentsResponse, UnallocatedPayment}
import uk.gov.hmrc.vapingdutyfinance.models.financialdata.*

import java.time.{Clock, LocalDate}
import javax.inject.{Inject, Singleton}
import scala.concurrent.{ExecutionContext, Future}

@Singleton
class FinancialDataService @Inject()(
                                      connector: FinancialDataConnector,
                                      appConfig: AppConfig,
                                      clock: Clock
                                    )(using ExecutionContext) extends Logging {

  // Overpayment/payment-on-account transaction code (matches Alcohol Duty's use of the same
  // code for its "Overpayment" transaction type) - only code relevant to VPD for now.
  private val unallocatedMainTransaction = "0060"
  private val vpdContractObjectType = "ZVPD"

  def getPayments(
                   vpdId: String,
                   dateFrom: Option[LocalDate],
                   dateTo: Option[LocalDate]
                 )(using HeaderCarrier): Future[PaymentsResponse] = {
    val effectiveDateFrom = dateFrom.getOrElse(appConfig.financialDataStartDate)
    val effectiveDateTo = dateTo.getOrElse(LocalDate.now(clock))

    val request = buildRequest(vpdId, effectiveDateFrom, effectiveDateTo)

    connector.getFinancialData(request)
      .map(response => transformToPayments(response))
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

  private def transformToPayments(response: FinancialDataResponse): PaymentsResponse = {
    val documents = response.success.financialData.flatMap(_.documentDetails).getOrElse(Seq.empty)

    val (unallocatedDocs, allocatedDocs) = documents.partition(isUnallocatedDocument)

    val outstanding = allocatedDocs
      .filter(_.documentOutstandingAmount.exists(_ > 0))
      .flatMap(toOutstandingPayments)

//    val cleared = allocatedDocs
//      .filter(_.documentClearedAmount.exists(_ > 0))
//      .flatMap(toClearedPayments)
//
//    val unallocated = unallocatedDocs.map(toUnallocatedPayment)

    PaymentsResponse(outstanding = outstanding, unallocated = Seq.empty, cleared = Seq.empty)
  }

  private def lineItems(doc: DocumentDetails): Seq[LineItemDetails] =
    doc.lineItemDetails.getOrElse(Seq.empty)

  private def isUnallocatedDocument(doc: DocumentDetails): Boolean =
    lineItems(doc).exists(_.mainTransaction.contains(unallocatedMainTransaction)) &&
    doc.contractObjectType.contains(vpdContractObjectType)

  private def toOutstandingPayments(doc: DocumentDetails): Seq[OutstandingPayment] =
    lineItems(doc).map { lineItem =>
      OutstandingPayment(
        chargeReference = doc.chargeReferenceNumber,
        periodFromDate = lineItem.periodFromDate,
        periodToDate = lineItem.periodToDate,
        amountDue = doc.documentOutstandingAmount.getOrElse(BigDecimal(0)),
        dueDate = lineItem.netDueDate,
        status = determineStatus(lineItem.netDueDate)
      )
    }

//  private def toClearedPayments(doc: DocumentDetails): Seq[ClearedPayment] =
//    lineItems(doc).map { lineItem =>
//      ClearedPayment(
//        chargeReference = doc.chargeReferenceNumber,
//        periodFromDate = lineItem.periodFromDate,
//        periodToDate = lineItem.periodToDate,
//        amountPaid = doc.documentClearedAmount.getOrElse(BigDecimal(0)),
//        clearedDate = lineItem.clearingDate
//      )
//    }
//
//  private def toUnallocatedPayment(doc: DocumentDetails): UnallocatedPayment =
//    UnallocatedPayment(
//      paymentReference = doc.documentNumber,
//      amount = doc.documentTotalAmount.getOrElse(BigDecimal(0)).abs,
//      paymentDate = doc.postingDate
//    )

  private def determineStatus(netDueDate: Option[LocalDate]): PaymentStatus = {
    netDueDate match {
      case Some(dueDate) if dueDate.isBefore(LocalDate.now(clock)) => PaymentStatus.Overdue
      case Some(_) => PaymentStatus.Due
      case None => PaymentStatus.Due
    }
  }

}
