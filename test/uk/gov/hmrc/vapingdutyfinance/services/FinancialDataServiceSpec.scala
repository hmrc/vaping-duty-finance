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

import org.mockito.ArgumentMatchers.any
import org.mockito.Mockito.when
import uk.gov.hmrc.http.UpstreamErrorResponse
import uk.gov.hmrc.vapingdutyfinance.base.SpecBase
import uk.gov.hmrc.vapingdutyfinance.connectors.FinancialDataConnector
import uk.gov.hmrc.vapingdutyfinance.models.PaymentStatus
import uk.gov.hmrc.vapingdutyfinance.models.financialdata.*

import java.time.{Instant, LocalDate}
import scala.concurrent.Future

class FinancialDataServiceSpec extends SpecBase {

  val mockConnector: FinancialDataConnector = mock[FinancialDataConnector]

  val service = FinancialDataService(mockConnector, appConfig, clock)

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

  val testResponse: FinancialDataResponse = FinancialDataResponse(
    success = FinancialDataSuccess(
      processingDate = Instant.parse("2026-10-01T10:15:10Z"),
      financialData = Some(FinancialData(
        totalisation = None,
        documentDetails = Some(Seq(testDocWithOutstanding))
      ))
    )
  )

  val testErrorResponse: FinancialDataErrorResponse = FinancialDataErrorResponse(
    errors = FinancialDataError(
      processingDate = Instant.parse("2026-10-01T10:15:10Z"),
      code = "018",
      text = "No data found"
    )
  )

  "FinancialDataService" - {
    "getOutstandingPayments must" - {
      "return outstanding payments when connector returns success" in {
        when(mockConnector.getFinancialData(any())(using any()))
          .thenReturn(Future.successful(Right(testResponse)))

        whenReady(service.getOutstandingPayments(testVpdId, Some(LocalDate.of(2024, 1, 1)), Some(LocalDate.of(2024, 12, 31)))) { result =>
          result.isRight mustBe true
          val payments = result.getOrElse(Seq.empty)
          payments must not be empty
          payments.head.chargeReference mustBe "XP001286394838"
          payments.head.amountDue mustBe BigDecimal("100.0")
        }
      }

      "filter out documents with no outstanding amount" in {
        val docWithoutOutstanding = testDocWithOutstanding.copy(
          documentOutstandingAmount = Some(BigDecimal("0.0")),
          chargeReferenceNumber = Some("DIFFERENT123")
        )
        
        val responseWithMixed = testResponse.copy(
          success = testResponse.success.copy(
            financialData = testResponse.success.financialData.map(fd =>
              fd.copy(documentDetails = Some(Seq(testDocWithOutstanding, docWithoutOutstanding)))
            )
          )
        )

        when(mockConnector.getFinancialData(any())(using any()))
          .thenReturn(Future.successful(Right(responseWithMixed)))

        whenReady(service.getOutstandingPayments(testVpdId, Some(LocalDate.of(2024, 1, 1)), Some(LocalDate.of(2024, 12, 31)))) { result =>
          result.isRight mustBe true
          val payments = result.getOrElse(Seq.empty)
          payments.size mustBe 1
          payments.head.chargeReference mustBe "XP001286394838"
        }
      }

      "return empty sequence when no documents exist" in {
        val emptyResponse = testResponse.copy(
          success = testResponse.success.copy(
            financialData = testResponse.success.financialData.map(fd =>
              fd.copy(documentDetails = None)
            )
          )
        )

        when(mockConnector.getFinancialData(any())(using any()))
          .thenReturn(Future.successful(Right(emptyResponse)))

        whenReady(service.getOutstandingPayments(testVpdId, Some(LocalDate.of(2024, 1, 1)), Some(LocalDate.of(2024, 12, 31)))) { result =>
          result.isRight mustBe true
          result.getOrElse(Seq.empty) mustBe empty
        }
      }

      "use default date range when dates not provided" in {
        when(mockConnector.getFinancialData(any())(using any()))
          .thenReturn(Future.successful(Right(testResponse)))

        whenReady(service.getOutstandingPayments(testVpdId, None, None)) { result =>
          result.isRight mustBe true
        }
      }

      "return error message when connector returns error response" in {
        when(mockConnector.getFinancialData(any())(using any()))
          .thenReturn(Future.successful(Left(testErrorResponse)))

        whenReady(service.getOutstandingPayments(testVpdId, Some(LocalDate.of(2024, 1, 1)), Some(LocalDate.of(2024, 12, 31)))) { result =>
          result.isLeft mustBe true
          result.left.getOrElse("") mustBe "No data found"
        }
      }

      Seq(
        ("002", "Invalid tax regime"),
        ("003", "Request could not be processed"),
        ("015", "Invalid ID type"),
        ("016", "Invalid ID number"),
        ("017", "Invalid search type or parameters"),
        ("018", "No data found"),
        ("019", "Invalid date type"),
        ("020", "Invalid date range"),
        ("135", "Duplicate submission reference"),
        ("999", "An error occurred while retrieving financial data")
      ).foreach { case (errorCode, expectedMessage) =>
        s"map error code $errorCode to correct message" in {
          val errorResponse = testErrorResponse.copy(
            errors = testErrorResponse.errors.copy(code = errorCode)
          )

          when(mockConnector.getFinancialData(any())(using any()))
            .thenReturn(Future.successful(Left(errorResponse)))

          whenReady(service.getOutstandingPayments(testVpdId, Some(LocalDate.of(2024, 1, 1)), Some(LocalDate.of(2024, 12, 31)))) { result =>
            result.isLeft mustBe true
            result.left.getOrElse("") mustBe expectedMessage
          }
        }
      }

      "return error message when connector fails" in {
        when(mockConnector.getFinancialData(any())(using any()))
          .thenReturn(Future.failed(UpstreamErrorResponse("Error", 500)))

        whenReady(service.getOutstandingPayments(testVpdId, Some(LocalDate.of(2024, 1, 1)), Some(LocalDate.of(2024, 12, 31)))) { result =>
          result.isLeft mustBe true
          result.left.getOrElse("") mustBe "An unexpected error occurred while retrieving financial data"
        }
      }

      "return Due status when due date is in the future" in {
        val futureDate = LocalDate.now(clock).plusDays(10)
        val docWithFutureDueDate = testDocWithOutstanding.copy(
          lineItemDetails = Some(Seq(
            testDocWithOutstanding.lineItemDetails.get.head.copy(
              netDueDate = Some(futureDate)
            )
          ))
        )

        val response = testResponse.copy(
          success = testResponse.success.copy(
            financialData = testResponse.success.financialData.map(fd =>
              fd.copy(documentDetails = Some(Seq(docWithFutureDueDate)))
            )
          )
        )

        when(mockConnector.getFinancialData(any())(using any()))
          .thenReturn(Future.successful(Right(response)))

        whenReady(service.getOutstandingPayments(testVpdId, Some(LocalDate.of(2024, 1, 1)), Some(LocalDate.of(2024, 12, 31)))) { result =>
          result.isRight mustBe true
          val payments = result.getOrElse(Seq.empty)
          payments.head.status mustBe PaymentStatus.Due
        }
      }

      "return Overdue status when due date is in the past" in {
        val pastDate = LocalDate.now(clock).minusDays(10)
        val docWithPastDueDate = testDocWithOutstanding.copy(
          lineItemDetails = Some(Seq(
            testDocWithOutstanding.lineItemDetails.get.head.copy(
              netDueDate = Some(pastDate)
            )
          ))
        )

        val response = testResponse.copy(
          success = testResponse.success.copy(
            financialData = testResponse.success.financialData.map(fd =>
              fd.copy(documentDetails = Some(Seq(docWithPastDueDate)))
            )
          )
        )

        when(mockConnector.getFinancialData(any())(using any()))
          .thenReturn(Future.successful(Right(response)))

        whenReady(service.getOutstandingPayments(testVpdId, Some(LocalDate.of(2024, 1, 1)), Some(LocalDate.of(2024, 12, 31)))) { result =>
          result.isRight mustBe true
          val payments = result.getOrElse(Seq.empty)
          payments.head.status mustBe PaymentStatus.Overdue
        }
      }

      "return Due status when due date is today" in {
        val today = LocalDate.now(clock)
        val docWithTodayDueDate = testDocWithOutstanding.copy(
          lineItemDetails = Some(Seq(
            testDocWithOutstanding.lineItemDetails.get.head.copy(
              netDueDate = Some(today)
            )
          ))
        )

        val response = testResponse.copy(
          success = testResponse.success.copy(
            financialData = testResponse.success.financialData.map(fd =>
              fd.copy(documentDetails = Some(Seq(docWithTodayDueDate)))
            )
          )
        )

        when(mockConnector.getFinancialData(any())(using any()))
          .thenReturn(Future.successful(Right(response)))

        whenReady(service.getOutstandingPayments(testVpdId, Some(LocalDate.of(2024, 1, 1)), Some(LocalDate.of(2024, 12, 31)))) { result =>
          result.isRight mustBe true
          val payments = result.getOrElse(Seq.empty)
          payments.head.status mustBe PaymentStatus.Due
        }
      }

      "return Due status when due date is None" in {
        val docWithNoDueDate = testDocWithOutstanding.copy(
          lineItemDetails = Some(Seq(
            testDocWithOutstanding.lineItemDetails.get.head.copy(
              netDueDate = None
            )
          ))
        )

        val response = testResponse.copy(
          success = testResponse.success.copy(
            financialData = testResponse.success.financialData.map(fd =>
              fd.copy(documentDetails = Some(Seq(docWithNoDueDate)))
            )
          )
        )

        when(mockConnector.getFinancialData(any())(using any()))
          .thenReturn(Future.successful(Right(response)))

        whenReady(service.getOutstandingPayments(testVpdId, Some(LocalDate.of(2024, 1, 1)), Some(LocalDate.of(2024, 12, 31)))) { result =>
          result.isRight mustBe true
          val payments = result.getOrElse(Seq.empty)
          payments.head.status mustBe PaymentStatus.Due
        }
      }

      "handle negative outstanding amounts correctly" in {
        val docWithNegativeAmount = testDocWithOutstanding.copy(
          documentOutstandingAmount = Some(BigDecimal("-150.75"))
        )

        val response = testResponse.copy(
          success = testResponse.success.copy(
            financialData = testResponse.success.financialData.map(fd =>
              fd.copy(documentDetails = Some(Seq(docWithNegativeAmount)))
            )
          )
        )

        when(mockConnector.getFinancialData(any())(using any()))
          .thenReturn(Future.successful(Right(response)))

        whenReady(service.getOutstandingPayments(testVpdId, Some(LocalDate.of(2024, 1, 1)), Some(LocalDate.of(2024, 12, 31)))) { result =>
          result.isRight mustBe true
          val payments = result.getOrElse(Seq.empty)
          payments.head.amountDue mustBe BigDecimal("-150.75")
        }
      }

      "preserve decimal precision without rounding" in {
        val preciseAmount = BigDecimal("100.123456789")
        val docWithPreciseAmount = testDocWithOutstanding.copy(
          documentOutstandingAmount = Some(preciseAmount)
        )

        val response = testResponse.copy(
          success = testResponse.success.copy(
            financialData = testResponse.success.financialData.map(fd =>
              fd.copy(documentDetails = Some(Seq(docWithPreciseAmount)))
            )
          )
        )

        when(mockConnector.getFinancialData(any())(using any()))
          .thenReturn(Future.successful(Right(response)))

        whenReady(service.getOutstandingPayments(testVpdId, Some(LocalDate.of(2024, 1, 1)), Some(LocalDate.of(2024, 12, 31)))) { result =>
          result.isRight mustBe true
          val payments = result.getOrElse(Seq.empty)
          payments.head.amountDue mustBe preciseAmount
        }
      }

      "create multiple outstanding payments when document has multiple line items" in {
        val lineItem1 = testDocWithOutstanding.lineItemDetails.get.head
        val lineItem2 = lineItem1.copy(
          itemNumber = Some("0002"),
          periodFromDate = Some(LocalDate.of(2026, 11, 1)),
          periodToDate = Some(LocalDate.of(2026, 11, 30))
        )

        val docWithMultipleLineItems = testDocWithOutstanding.copy(
          lineItemDetails = Some(Seq(lineItem1, lineItem2))
        )

        val response = testResponse.copy(
          success = testResponse.success.copy(
            financialData = testResponse.success.financialData.map(fd =>
              fd.copy(documentDetails = Some(Seq(docWithMultipleLineItems)))
            )
          )
        )

        when(mockConnector.getFinancialData(any())(using any()))
          .thenReturn(Future.successful(Right(response)))

        whenReady(service.getOutstandingPayments(testVpdId, Some(LocalDate.of(2024, 1, 1)), Some(LocalDate.of(2024, 12, 31)))) { result =>
          result.isRight mustBe true
          val payments = result.getOrElse(Seq.empty)
          payments.size mustBe 2
          payments.head.chargeReference mustBe "XP001286394838"
          payments(1).chargeReference mustBe "XP001286394838"
          payments.head.period must not be payments(1).period
        }
      }

      "return empty sequence when document has empty line items" in {
        val docWithEmptyLineItems = testDocWithOutstanding.copy(
          lineItemDetails = Some(Seq.empty)
        )

        val response = testResponse.copy(
          success = testResponse.success.copy(
            financialData = testResponse.success.financialData.map(fd =>
              fd.copy(documentDetails = Some(Seq(docWithEmptyLineItems)))
            )
          )
        )

        when(mockConnector.getFinancialData(any())(using any()))
          .thenReturn(Future.successful(Right(response)))

        whenReady(service.getOutstandingPayments(testVpdId, Some(LocalDate.of(2024, 1, 1)), Some(LocalDate.of(2024, 12, 31)))) { result =>
          result.isRight mustBe true
          result.getOrElse(Seq.empty) mustBe empty
        }
      }

      "return empty sequence when document has no line items" in {
        val docWithNoLineItems = testDocWithOutstanding.copy(
          lineItemDetails = None
        )

        val response = testResponse.copy(
          success = testResponse.success.copy(
            financialData = testResponse.success.financialData.map(fd =>
              fd.copy(documentDetails = Some(Seq(docWithNoLineItems)))
            )
          )
        )

        when(mockConnector.getFinancialData(any())(using any()))
          .thenReturn(Future.successful(Right(response)))

        whenReady(service.getOutstandingPayments(testVpdId, Some(LocalDate.of(2024, 1, 1)), Some(LocalDate.of(2024, 12, 31)))) { result =>
          result.isRight mustBe true
          result.getOrElse(Seq.empty) mustBe empty
        }
      }

      "default to 'Unknown' when charge reference is None" in {
        val docWithNoChargeRef = testDocWithOutstanding.copy(
          chargeReferenceNumber = None
        )

        val response = testResponse.copy(
          success = testResponse.success.copy(
            financialData = testResponse.success.financialData.map(fd =>
              fd.copy(documentDetails = Some(Seq(docWithNoChargeRef)))
            )
          )
        )

        when(mockConnector.getFinancialData(any())(using any()))
          .thenReturn(Future.successful(Right(response)))

        whenReady(service.getOutstandingPayments(testVpdId, Some(LocalDate.of(2024, 1, 1)), Some(LocalDate.of(2024, 12, 31)))) { result =>
          result.isRight mustBe true
          val payments = result.getOrElse(Seq.empty)
          payments.head.chargeReference mustBe "Unknown"
        }
      }

      "default to 0 when outstanding amount is None" in {
        val docWithNoOutstandingAmount = testDocWithOutstanding.copy(
          documentOutstandingAmount = None
        )

        val response = testResponse.copy(
          success = testResponse.success.copy(
            financialData = testResponse.success.financialData.map(fd =>
              fd.copy(documentDetails = Some(Seq(docWithNoOutstandingAmount)))
            )
          )
        )

        when(mockConnector.getFinancialData(any())(using any()))
          .thenReturn(Future.successful(Right(response)))

        whenReady(service.getOutstandingPayments(testVpdId, Some(LocalDate.of(2024, 1, 1)), Some(LocalDate.of(2024, 12, 31)))) { result =>
          result.isRight mustBe true
          result.getOrElse(Seq.empty) mustBe empty
        }
      }

      "format period as 'From YYYY-MM-DD' when only periodFromDate is present" in {
        val docWithOnlyFromDate = testDocWithOutstanding.copy(
          lineItemDetails = Some(Seq(
            testDocWithOutstanding.lineItemDetails.get.head.copy(
              periodFromDate = Some(LocalDate.of(2026, 10, 1)),
              periodToDate = None
            )
          ))
        )

        val response = testResponse.copy(
          success = testResponse.success.copy(
            financialData = testResponse.success.financialData.map(fd =>
              fd.copy(documentDetails = Some(Seq(docWithOnlyFromDate)))
            )
          )
        )

        when(mockConnector.getFinancialData(any())(using any()))
          .thenReturn(Future.successful(Right(response)))

        whenReady(service.getOutstandingPayments(testVpdId, Some(LocalDate.of(2024, 1, 1)), Some(LocalDate.of(2024, 12, 31)))) { result =>
          result.isRight mustBe true
          val payments = result.getOrElse(Seq.empty)
          payments.head.period mustBe "From 2026-10-01"
        }
      }

      "format period as 'To YYYY-MM-DD' when only periodToDate is present" in {
        val docWithOnlyToDate = testDocWithOutstanding.copy(
          lineItemDetails = Some(Seq(
            testDocWithOutstanding.lineItemDetails.get.head.copy(
              periodFromDate = None,
              periodToDate = Some(LocalDate.of(2026, 12, 31))
            )
          ))
        )

        val response = testResponse.copy(
          success = testResponse.success.copy(
            financialData = testResponse.success.financialData.map(fd =>
              fd.copy(documentDetails = Some(Seq(docWithOnlyToDate)))
            )
          )
        )

        when(mockConnector.getFinancialData(any())(using any()))
          .thenReturn(Future.successful(Right(response)))

        whenReady(service.getOutstandingPayments(testVpdId, Some(LocalDate.of(2024, 1, 1)), Some(LocalDate.of(2024, 12, 31)))) { result =>
          result.isRight mustBe true
          val payments = result.getOrElse(Seq.empty)
          payments.head.period mustBe "To 2026-12-31"
        }
      }

      "format period as 'Unknown period' when both dates are None" in {
        val docWithNoDates = testDocWithOutstanding.copy(
          lineItemDetails = Some(Seq(
            testDocWithOutstanding.lineItemDetails.get.head.copy(
              periodFromDate = None,
              periodToDate = None
            )
          ))
        )

        val response = testResponse.copy(
          success = testResponse.success.copy(
            financialData = testResponse.success.financialData.map(fd =>
              fd.copy(documentDetails = Some(Seq(docWithNoDates)))
            )
          )
        )

        when(mockConnector.getFinancialData(any())(using any()))
          .thenReturn(Future.successful(Right(response)))

        whenReady(service.getOutstandingPayments(testVpdId, Some(LocalDate.of(2024, 1, 1)), Some(LocalDate.of(2024, 12, 31)))) { result =>
          result.isRight mustBe true
          val payments = result.getOrElse(Seq.empty)
          payments.head.period mustBe "Unknown period"
        }
      }

      "handle different period key formats" in {
        Seq("26KJ", "24AA", "25BC", "27XY").foreach { periodKey =>
          val docWithPeriodKey = testDocWithOutstanding.copy(
            lineItemDetails = Some(Seq(
              testDocWithOutstanding.lineItemDetails.get.head.copy(
                periodKey = Some(periodKey)
              )
            ))
          )

          val response = testResponse.copy(
            success = testResponse.success.copy(
              financialData = testResponse.success.financialData.map(fd =>
                fd.copy(documentDetails = Some(Seq(docWithPeriodKey)))
              )
            )
          )

          when(mockConnector.getFinancialData(any())(using any()))
            .thenReturn(Future.successful(Right(response)))

          whenReady(service.getOutstandingPayments(testVpdId, Some(LocalDate.of(2024, 1, 1)), Some(LocalDate.of(2024, 12, 31)))) { result =>
            result.isRight mustBe true
            val payments = result.getOrElse(Seq.empty)
            payments must not be empty
          }
        }
      }

      "handle multiple documents with outstanding amounts" in {
        val doc1 = testDocWithOutstanding.copy(
          chargeReferenceNumber = Some("XP001286394838"),
          documentOutstandingAmount = Some(BigDecimal("100.0"))
        )
        val doc2 = testDocWithOutstanding.copy(
          chargeReferenceNumber = Some("XP001286394839"),
          documentOutstandingAmount = Some(BigDecimal("200.0"))
        )
        val doc3 = testDocWithOutstanding.copy(
          chargeReferenceNumber = Some("XP001286394840"),
          documentOutstandingAmount = Some(BigDecimal("300.0"))
        )

        val response = testResponse.copy(
          success = testResponse.success.copy(
            financialData = testResponse.success.financialData.map(fd =>
              fd.copy(documentDetails = Some(Seq(doc1, doc2, doc3)))
            )
          )
        )

        when(mockConnector.getFinancialData(any())(using any()))
          .thenReturn(Future.successful(Right(response)))

        whenReady(service.getOutstandingPayments(testVpdId, Some(LocalDate.of(2024, 1, 1)), Some(LocalDate.of(2024, 12, 31)))) { result =>
          result.isRight mustBe true
          val payments = result.getOrElse(Seq.empty)
          payments.size mustBe 3
          payments.map(_.amountDue) must contain allOf (BigDecimal("100.0"), BigDecimal("200.0"), BigDecimal("300.0"))
        }
      }
    }
  }
}
