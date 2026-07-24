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

package uk.gov.hmrc.vapingdutyfinance.base

import uk.gov.hmrc.vapingdutyfinance.models.{ClearedPayment, OutstandingPayment, PaymentStatus, PaymentsResponse, UnallocatedPayment}
import uk.gov.hmrc.vapingdutyfinance.models.financialdata.{DocumentDetails, FinancialData, FinancialDataResponse, FinancialDataSuccess, LineItemDetails}

import java.time.{Clock, Instant, LocalDate, ZoneId}

trait TestData {
  val clock: Clock = Clock.fixed(Instant.ofEpochMilli(1718118467838L), ZoneId.of("UTC"))

  val testVpdId = "GBWK9653452WK"
  val testUserId = "test-user-id"
  val testCorrelationId = "f0bD1f32-de51-45cc-9B18-0520d6e3ab1a"

  // Payment fixtures shared between FinancialDataServiceSpec and FinancialDataControllerSpec.
  // testDocWithOutstanding/testDocWithCleared/testDocUnallocated are the downstream API shape,
  // testOutstandingPayment/testClearedPayment/testUnallocatedPayment are what they transform into.
  val testDocWithOutstanding: DocumentDetails = DocumentDetails(
    documentNumber = Some("187346702498"),
    documentType = Some("TRM New Charge"),
    chargeReferenceNumber = Some("XP001286394838"),
    businessPartnerNumber = Some("100893731"),
    contractAccountNumber = Some("900726630"),
    contractAccountCategory = Some("Excise"),
    contractObjectNumber = Some("104920928302302"),
    contractObjectType = Some("ZVPD"),
    postingDate = Some(LocalDate.of(2026, 10, 1)),
    issueDate = Some(LocalDate.of(2026, 10, 1)),
    documentTotalAmount = Some(BigDecimal("100.0")),
    documentClearedAmount = Some(BigDecimal("0.0")),
    documentOutstandingAmount = Some(BigDecimal("100.0")),
    documentInterestTotals = None,
    documentPenaltyTotals = None,
    lineItemDetails = Some(Seq(LineItemDetails(
      itemNumber = Some("0001"),
      subItemNumber = Some("003"),
      mainTransaction = Some("4060"),
      subTransaction = Some("3392"),
      chargeDescription = Some("VPD Return"),
      periodFromDate = Some(LocalDate.of(2026, 10, 1)),
      periodToDate = Some(LocalDate.of(2026, 12, 31)),
      periodKey = Some("26KJ"),
      netDueDate = Some(LocalDate.of(2026, 10, 1)),
      formBundleNumber = Some("125435934761"),
      statisticalKey = Some("1"),
      amount = Some(BigDecimal("3420.0")),
      clearingDate = None,
      clearingReason = None,
      clearingDocument = None,
      outgoingPaymentMethod = Some("B"),
      ddCollectionInProgress = Some(true)
    )))
  )

  val testDocWithCleared: DocumentDetails = testDocWithOutstanding.copy(
    documentNumber = Some("187346702499"),
    chargeReferenceNumber = Some("XP001286394839"),
    documentClearedAmount = Some(BigDecimal("100.0")),
    documentOutstandingAmount = Some(BigDecimal("0.0")),
    lineItemDetails = Some(Seq(
      testDocWithOutstanding.lineItemDetails.get.head.copy(
        clearingDate = Some(LocalDate.of(2026, 10, 5))
      )
    ))
  )

  val testDocUnallocated: DocumentDetails = testDocWithOutstanding.copy(
    documentNumber = Some("187346702500"),
    chargeReferenceNumber = None,
    documentTotalAmount = Some(BigDecimal("50.0")),
    documentOutstandingAmount = None,
    documentClearedAmount = None,
    lineItemDetails = Some(Seq(
      testDocWithOutstanding.lineItemDetails.get.head.copy(mainTransaction = Some("0060"))
    ))
  )

  val testResponse: FinancialDataResponse = FinancialDataResponse(
    success = FinancialDataSuccess(
      processingDate = Instant.parse("2026-10-01T10:15:10Z"),
      financialData = Some(FinancialData(
        totalisation = None,
        documentDetails = Some(Seq(testDocWithOutstanding))
      ))
    )
  )

  val testOutstandingPayment: OutstandingPayment = OutstandingPayment(chargeReference = "XP001286394838", period = "2026-10-01 to 2026-12-31", amountDue = BigDecimal("100.0"), dueDate = "2026-10-01", status = PaymentStatus.Due)
  val testUnallocatedPayment: UnallocatedPayment = UnallocatedPayment(paymentReference = "187346702500", amount = BigDecimal("50.0"), paymentDate = "2026-10-01")
  val testClearedPayment: ClearedPayment = ClearedPayment(chargeReference = "XP001286394839", period = "2026-10-01 to 2026-12-31", amountPaid = BigDecimal("100.0"), clearedDate = "2026-10-05")

  val testPaymentsResponse: PaymentsResponse = PaymentsResponse(
    outstanding = Seq(testOutstandingPayment),
    unallocated = Seq(testUnallocatedPayment),
    cleared = Seq(testClearedPayment)
  )
}
