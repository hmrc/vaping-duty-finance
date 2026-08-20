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
import uk.gov.hmrc.vapingdutyfinance.models.financialdata.*
import uk.gov.hmrc.vapingdutyfinance.models.{ClearedPayment, MainTransactionType, OutstandingPayment, PaymentOnAccount, PaymentStatus, PaymentsResponse}

import java.time.{Clock, LocalDate}
import javax.inject.{Inject, Singleton}
import scala.concurrent.{ExecutionContext, Future}

@Singleton
class FinancialDataService @Inject()(
                                      connector: FinancialDataConnector,
                                      appConfig: AppConfig,
                                      clock: Clock
                                    )(using ExecutionContext) extends Logging {

  def getPayments(
                   vpdId: String,
                   dateFrom: Option[LocalDate],
                   dateTo: Option[LocalDate]
                 )(using HeaderCarrier): Future[PaymentsResponse] = {
    val effectiveDateFrom = dateFrom.getOrElse(appConfig.financialDataStartDate)
    val effectiveDateTo = dateTo.getOrElse(LocalDate.now(clock))

    connector.getFinancialData(vpdId, effectiveDateFrom, effectiveDateTo)
      .map(response => transformToPayments(response))
  }

  private def transformToPayments(response: FinancialDataResponse): PaymentsResponse = {
    val documents = response.success.financialData.flatMap(_.documentDetails).getOrElse(Seq.empty)
    val totalAccountBalance = response.success.financialData
      .flatMap(_.totalisation)
      .flatMap(_.regimeTotalisation)
      .flatMap(_.totalAccountBalance)

    val (paymentOnAccountDocs, outstandingAndCleared) = documents.partition(isPaymentOnAccountDocument)

    val outstanding = outstandingAndCleared
      .filter(_.documentOutstandingAmount.exists(_ > 0))
      .flatMap(toOutstandingPayments)

    val cleared = outstandingAndCleared
      .filter(_.documentClearedAmount.exists(_ > 0))
      .flatMap(toClearedPayments)

    val paymentOnAccount = paymentOnAccountDocs.map(toPaymentOnAccount)

    PaymentsResponse(
      outstanding = outstanding,
      paymentOnAccount = paymentOnAccount,
      cleared = cleared,
      totalAccountBalance = totalAccountBalance
    )
  }

  private def lineItems(doc: DocumentDetails): Seq[LineItemDetails] =
    doc.lineItemDetails.getOrElse(Seq.empty)

  private def isPaymentOnAccount(lineItem: LineItemDetails): Boolean =
    lineItem.mainTransaction.contains(MainTransactionType.PaymentOnAccount.code)

  private def isVpdRegime(doc: DocumentDetails): Boolean =
    doc.contractObjectType.contains("ZVPD")

  private def isPaymentOnAccountDocument(doc: DocumentDetails): Boolean =
    lineItems(doc).exists(isPaymentOnAccount) && isVpdRegime(doc)

  private def toOutstandingPayments(doc: DocumentDetails): Seq[OutstandingPayment] =
    lineItems(doc).headOption.map { firstLineItem =>
      OutstandingPayment(
        chargeReference = doc.chargeReferenceNumber,
        amountDue       = doc.documentOutstandingAmount.getOrElse(BigDecimal(0)),
        dueDate         = firstLineItem.netDueDate,
        status          = determineStatus(firstLineItem.netDueDate),
        mainTransaction = firstLineItem.mainTransaction
      )
    }.toSeq

  private def toClearedPayments(doc: DocumentDetails): Seq[ClearedPayment] =
    Seq(
      ClearedPayment(
        chargeReference = doc.chargeReferenceNumber,
        amountPaid      = doc.documentClearedAmount.getOrElse(BigDecimal(0)),
        clearedDate     = mostRecentPaymentDate(doc)
      )
    )

  private def mostRecentPaymentDate(doc: DocumentDetails): Option[LocalDate] = {
    lineItems(doc)
      .flatMap(_.clearingDate)
      .maxOption
  }

  private def toPaymentOnAccount(doc: DocumentDetails): PaymentOnAccount =
    PaymentOnAccount(
      amount = doc.documentTotalAmount.getOrElse(BigDecimal(0)).abs,
      paymentDate = doc.postingDate
    )

  private def determineStatus(netDueDate: Option[LocalDate]): PaymentStatus = {
    netDueDate match {
      case Some(dueDate) if dueDate.isBefore(LocalDate.now(clock)) => PaymentStatus.Overdue
      case Some(_) => PaymentStatus.Due
      case None => PaymentStatus.Due
    }
  }
}
