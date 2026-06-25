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
import play.api.http.Status.INTERNAL_SERVER_ERROR
import uk.gov.hmrc.http.UpstreamErrorResponse
import uk.gov.hmrc.vapingdutyfinance.base.SpecBase
import uk.gov.hmrc.vapingdutyfinance.config.AppConfig
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
          .thenReturn(Future.successful(testResponse))

        whenReady(service.getOutstandingPayments(testVpdId, Some(LocalDate.of(2024, 1, 1)), Some(LocalDate.of(2024, 12, 31)))) { result =>
          result must not be empty
          result.head.chargeReference mustBe "XP001286394838"
          result.head.amountDue mustBe BigDecimal("100.0")
        }
      }

      "return NothingToPay payment when no documents exist" in {
        val emptyResponse = testResponse.copy(
          success = testResponse.success.copy(
            financialData = testResponse.success.financialData.map(fd =>
              fd.copy(documentDetails = None)
            )
          )
        )

        when(mockConnector.getFinancialData(any())(using any()))
          .thenReturn(Future.successful(emptyResponse))

        whenReady(service.getOutstandingPayments(testVpdId, Some(LocalDate.of(2024, 1, 1)), Some(LocalDate.of(2024, 12, 31)))) { result =>
          result.size mustBe 1
          result.head.chargeReference mustBe ""
          result.head.period mustBe ""
          result.head.amountDue mustBe BigDecimal(0)
          result.head.dueDate mustBe ""
          result.head.status mustBe PaymentStatus.NothingToPay
        }
      }

      "use default date range when dates not provided" in {
        when(mockConnector.getFinancialData(any())(using any()))
          .thenReturn(Future.successful(testResponse))

        whenReady(service.getOutstandingPayments(testVpdId, None, None)) { result =>
          result must not be empty
        }
      }

      "propagate failure when connector fails" in {
        val exception = UpstreamErrorResponse("API error", INTERNAL_SERVER_ERROR)
        when(mockConnector.getFinancialData(any())(using any()))
          .thenReturn(Future.failed(exception))

        whenReady(service.getOutstandingPayments(testVpdId, Some(LocalDate.of(2024, 1, 1)), Some(LocalDate.of(2024, 12, 31))).failed) { ex =>
          ex mustBe exception
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
          .thenReturn(Future.successful(response))

        whenReady(service.getOutstandingPayments(testVpdId, Some(LocalDate.of(2024, 1, 1)), Some(LocalDate.of(2024, 12, 31)))) { result =>
          result.head.status mustBe PaymentStatus.Due
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
          .thenReturn(Future.successful(response))

        whenReady(service.getOutstandingPayments(testVpdId, Some(LocalDate.of(2024, 1, 1)), Some(LocalDate.of(2024, 12, 31)))) { result =>
          result.head.status mustBe PaymentStatus.Overdue
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
          .thenReturn(Future.successful(response))

        whenReady(service.getOutstandingPayments(testVpdId, Some(LocalDate.of(2024, 1, 1)), Some(LocalDate.of(2024, 12, 31)))) { result =>
          result.head.status mustBe PaymentStatus.Due
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
          .thenReturn(Future.successful(response))

        whenReady(service.getOutstandingPayments(testVpdId, Some(LocalDate.of(2024, 1, 1)), Some(LocalDate.of(2024, 12, 31)))) { result =>
          result.head.status mustBe PaymentStatus.Due
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
          .thenReturn(Future.successful(response))

        whenReady(service.getOutstandingPayments(testVpdId, Some(LocalDate.of(2024, 1, 1)), Some(LocalDate.of(2024, 12, 31)))) { result =>
          result.head.amountDue mustBe BigDecimal("-150.75")
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
          .thenReturn(Future.successful(response))

        whenReady(service.getOutstandingPayments(testVpdId, Some(LocalDate.of(2024, 1, 1)), Some(LocalDate.of(2024, 12, 31)))) { result =>
          result.head.amountDue mustBe preciseAmount
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
          .thenReturn(Future.successful(response))

        whenReady(service.getOutstandingPayments(testVpdId, Some(LocalDate.of(2024, 1, 1)), Some(LocalDate.of(2024, 12, 31)))) { result =>
          result.size mustBe 2
        }
      }
    }

    "when using static financial data must" - {
      "return static payments when useStaticFinancialData is true" in {
        val mockAppConfigWithStatic = mock[uk.gov.hmrc.vapingdutyfinance.config.AppConfig]
        when(mockAppConfigWithStatic.useStaticFinancialData).thenReturn(true)
        
        val serviceWithStaticData = FinancialDataService(mockConnector, mockAppConfigWithStatic, clock)

        whenReady(serviceWithStaticData.getOutstandingPayments(testVpdId, Some(LocalDate.of(2024, 1, 1)), Some(LocalDate.of(2024, 12, 31)))) { payments =>
          payments.size mustBe 3
        }
      }

      "return first static payment with correct values" in {
        val mockAppConfigWithStatic = mock[AppConfig]
        when(mockAppConfigWithStatic.useStaticFinancialData).thenReturn(true)
        
        val serviceWithStaticData = FinancialDataService(mockConnector, mockAppConfigWithStatic, clock)

        whenReady(serviceWithStaticData.getOutstandingPayments(testVpdId, None, None)) { payments =>
          val firstPayment = payments.head
          
          firstPayment.chargeReference mustBe "XM002610011594"
          firstPayment.period mustBe "2024-01-01 to 2024-01-31"
          firstPayment.amountDue mustBe BigDecimal("1250.50")
          firstPayment.dueDate mustBe "2024-02-15"
          firstPayment.status mustBe PaymentStatus.Overdue
        }
      }

      "ignore date parameters when using static data" in {
        val mockAppConfigWithStatic = mock[uk.gov.hmrc.vapingdutyfinance.config.AppConfig]
        when(mockAppConfigWithStatic.useStaticFinancialData).thenReturn(true)
        
        val serviceWithStaticData = FinancialDataService(mockConnector, mockAppConfigWithStatic, clock)

        // Call with different date parameters
        val result1 = serviceWithStaticData.getOutstandingPayments(testVpdId, Some(LocalDate.of(2020, 1, 1)), Some(LocalDate.of(2020, 12, 31)))
        val result2 = serviceWithStaticData.getOutstandingPayments(testVpdId, Some(LocalDate.of(2025, 1, 1)), Some(LocalDate.of(2025, 12, 31)))
        val result3 = serviceWithStaticData.getOutstandingPayments(testVpdId, None, None)

        whenReady(result1) { payments1 =>
          whenReady(result2) { payments2 =>
            whenReady(result3) { payments3 =>
              // All should return the same static data regardless of dates
              payments1 mustBe payments2
              payments2 mustBe payments3
            }
          }
        }
      }
    }

    "formatPeriod must" - {
      "format period with only from date" in {
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
          .thenReturn(Future.successful(response))

        whenReady(service.getOutstandingPayments(testVpdId, Some(LocalDate.of(2024, 1, 1)), Some(LocalDate.of(2024, 12, 31)))) { result =>
          result.head.period mustBe "From 2026-10-01"
        }
      }

      "format period with only to date" in {
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
          .thenReturn(Future.successful(response))

        whenReady(service.getOutstandingPayments(testVpdId, Some(LocalDate.of(2024, 1, 1)), Some(LocalDate.of(2024, 12, 31)))) { result =>
          result.head.period mustBe "To 2026-12-31"
        }
      }

      "format period with no dates" in {
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
          .thenReturn(Future.successful(response))

        whenReady(service.getOutstandingPayments(testVpdId, Some(LocalDate.of(2024, 1, 1)), Some(LocalDate.of(2024, 12, 31)))) { result =>
          result.head.period mustBe "Unknown period"
        }
      }
    }

    "zero amount handling must" - {
      "return NothingToPay payment when all amounts are zero" in {
        val docWithZeroAmount = testDocWithOutstanding.copy(
          documentOutstandingAmount = Some(BigDecimal(0))
        )

        val response = testResponse.copy(
          success = testResponse.success.copy(
            financialData = testResponse.success.financialData.map(fd =>
              fd.copy(documentDetails = Some(Seq(docWithZeroAmount)))
            )
          )
        )

        when(mockConnector.getFinancialData(any())(using any()))
          .thenReturn(Future.successful(response))

        whenReady(service.getOutstandingPayments(testVpdId, Some(LocalDate.of(2024, 1, 1)), Some(LocalDate.of(2024, 12, 31)))) { result =>
          result.size mustBe 1
          result.head.chargeReference mustBe ""
          result.head.period mustBe ""
          result.head.amountDue mustBe BigDecimal(0)
          result.head.dueDate mustBe ""
          result.head.status mustBe PaymentStatus.NothingToPay
        }
      }
    }
  }
}
