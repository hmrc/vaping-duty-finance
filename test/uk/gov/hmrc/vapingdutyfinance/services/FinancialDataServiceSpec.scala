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

import org.mockito.ArgumentCaptor
import org.mockito.ArgumentMatchers.any
import org.mockito.Mockito.{atLeastOnce, verify, when}
import play.api.http.Status.INTERNAL_SERVER_ERROR
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

  "FinancialDataService" - {
    "getPayments must" - {
      "return outstanding payments when connector returns success" in {
        when(mockConnector.getFinancialData(any(), any(), any())(using any()))
          .thenReturn(Future.successful(testResponse))

        whenReady(service.getPayments(testVpdId, Some(LocalDate.of(2024, 1, 1)), Some(LocalDate.of(2024, 12, 31)))) { result =>
          result.outstanding must not be empty
          result.outstanding.head.chargeReference mustBe Some("XP001286394838")
          result.outstanding.head.amountDue mustBe BigDecimal("100.0")
          result.paymentOnAccount mustBe empty
          result.cleared mustBe empty
          result.totalisation mustBe sampleTotalisation
        }
      }

      "pass through totalisation when present in response" in {
        when(mockConnector.getFinancialData(any(), any(), any())(using any()))
          .thenReturn(Future.successful(testResponse))

        whenReady(service.getPayments(testVpdId, Some(LocalDate.of(2024, 1, 1)), Some(LocalDate.of(2024, 12, 31)))) { result =>
          result.totalisation mustBe sampleTotalisation
          result.totalisation.regimeTotalisation mustBe Some(sampleRegimeTotalisation)
          result.totalisation.regimeTotalisation.get.totalAccountOverdue mustBe BigDecimal("100.0")
          result.totalisation.regimeTotalisation.get.totalAccountNotYetDue mustBe BigDecimal("200.0")
          result.totalisation.regimeTotalisation.get.totalAccountCredit mustBe BigDecimal("0.0")
          result.totalisation.regimeTotalisation.get.totalAccountBalance mustBe BigDecimal("300.0")
        }
      }

      "return empty totalisation when not present in response" in {
        val responseWithoutTotalisation = testResponse.copy(
          success = testResponse.success.copy(
            financialData = testResponse.success.financialData.map(fd =>
              fd.copy(totalisation = None)
            )
          )
        )

        when(mockConnector.getFinancialData(any(), any(), any())(using any()))
          .thenReturn(Future.successful(responseWithoutTotalisation))

        whenReady(service.getPayments(testVpdId, Some(LocalDate.of(2024, 1, 1)), Some(LocalDate.of(2024, 12, 31)))) { result =>
          result.totalisation mustBe Totalisation(None)
          result.totalisation.regimeTotalisation mustBe None
        }
      }

//      "return cleared payments for documents with a cleared amount" in {
//        val response = testResponse.copy(
//          success = testResponse.success.copy(
//            financialData = Some(FinancialData(totalisation = None, documentDetails = Some(Seq(testDocWithCleared))))
//          )
//        )
//        when(mockConnector.getFinancialData(any(), any(), any())(using any()))
//          .thenReturn(Future.successful(response))
//
//        whenReady(service.getPayments(testVpdId, Some(LocalDate.of(2024, 1, 1)), Some(LocalDate.of(2024, 12, 31)))) { result =>
//          result.outstanding mustBe empty
//          result.paymentOnAccount mustBe empty
//          result.cleared must not be empty
//          result.cleared.head.chargeReference mustBe Some("XP001286394839")
//          result.cleared.head.amountPaid mustBe BigDecimal("100.0")
//          result.cleared.head.clearedDate mustBe Some(LocalDate.of(2026, 10, 5))
//        }
//      }
//
//      "return payment on account payments for documents with main transaction 0060" in {
//        val response = testResponse.copy(
//          success = testResponse.success.copy(
//            financialData = Some(FinancialData(totalisation = None, documentDetails = Some(Seq(testDocUnallocated))))
//          )
//        )
//        when(mockConnector.getFinancialData(any(), any(), any())(using any()))
//          .thenReturn(Future.successful(response))
//
//        whenReady(service.getPayments(testVpdId, Some(LocalDate.of(2024, 1, 1)), Some(LocalDate.of(2024, 12, 31)))) { result =>
//          result.outstanding mustBe empty
//          result.cleared mustBe empty
//          result.paymentOnAccount must not be empty
//          result.paymentOnAccount.head.paymentReference mustBe Some("187346702500")
//          result.paymentOnAccount.head.amount mustBe BigDecimal("50.0")
//          result.paymentOnAccount.head.paymentDate mustBe Some(LocalDate.of(2026, 10, 1))
//        }
//      }

      "return only outstanding payments when a mix of outstanding, payment on account and cleared documents are present" in {
        val response = testResponse.copy(
          success = testResponse.success.copy(
            financialData = Some(FinancialData(
              totalisation = Some(sampleTotalisation),
              documentDetails = Some(Seq(testDocWithOutstanding, testDocWithCleared, testDocUnallocated))
            ))
          )
        )
        when(mockConnector.getFinancialData(any(), any(), any())(using any()))
          .thenReturn(Future.successful(response))

        whenReady(service.getPayments(testVpdId, Some(LocalDate.of(2024, 1, 1)), Some(LocalDate.of(2024, 12, 31)))) { result =>
          result.outstanding.size mustBe 1
          result.cleared mustBe empty
          result.paymentOnAccount mustBe empty
        }
      }

      "drop a document with an outstanding/cleared amount but no line items" in {
        val docWithNoLineItems = testDocWithOutstanding.copy(lineItemDetails = None)

        val response = testResponse.copy(
          success = testResponse.success.copy(
            financialData = Some(FinancialData(totalisation = Some(sampleTotalisation), documentDetails = Some(Seq(docWithNoLineItems))))
          )
        )

        when(mockConnector.getFinancialData(any(), any(), any())(using any()))
          .thenReturn(Future.successful(response))

        whenReady(service.getPayments(testVpdId, Some(LocalDate.of(2024, 1, 1)), Some(LocalDate.of(2024, 12, 31)))) { result =>
          result.outstanding mustBe empty
          result.paymentOnAccount mustBe empty
          result.cleared mustBe empty
        }
      }

      "return the outstanding amount for a document with a partial payment" in {
        val docWithPartialPayment = testDocWithOutstanding.copy(
          documentOutstandingAmount = Some(BigDecimal("40.0")),
          documentClearedAmount = Some(BigDecimal("60.0")),
          lineItemDetails = Some(Seq(
            testDocWithOutstanding.lineItemDetails.get.head.copy(clearingDate = Some(LocalDate.of(2026, 10, 5)))
          ))
        )

        val response = testResponse.copy(
          success = testResponse.success.copy(
            financialData = Some(FinancialData(totalisation = Some(sampleTotalisation), documentDetails = Some(Seq(docWithPartialPayment))))
          )
        )

        when(mockConnector.getFinancialData(any(), any(), any())(using any()))
          .thenReturn(Future.successful(response))

        whenReady(service.getPayments(testVpdId, Some(LocalDate.of(2024, 1, 1)), Some(LocalDate.of(2024, 12, 31)))) { result =>
          result.outstanding must not be empty
          result.outstanding.head.amountDue mustBe BigDecimal("40.0")
          result.cleared mustBe empty
          result.paymentOnAccount mustBe empty
        }
      }

      "return an entirely empty PaymentsResponse when no documents exist" in {
        val emptyResponse = testResponse.copy(
          success = testResponse.success.copy(
            financialData = testResponse.success.financialData.map(fd =>
              fd.copy(documentDetails = None)
            )
          )
        )

        when(mockConnector.getFinancialData(any(), any(), any())(using any()))
          .thenReturn(Future.successful(emptyResponse))

        whenReady(service.getPayments(testVpdId, Some(LocalDate.of(2024, 1, 1)), Some(LocalDate.of(2024, 12, 31)))) { result =>
          result.outstanding mustBe empty
          result.paymentOnAccount mustBe empty
          result.cleared mustBe empty
        }
      }

      "return an entirely empty PaymentsResponse when financialData itself is absent (018/no-data case)" in {
        val noDataResponse = FinancialDataResponse(
          success = FinancialDataSuccess(processingDate = Instant.parse("2026-10-01T10:15:10Z"), financialData = None)
        )

        when(mockConnector.getFinancialData(any(), any(), any())(using any()))
          .thenReturn(Future.successful(noDataResponse))

        whenReady(service.getPayments(testVpdId, None, None)) { result =>
          result.outstanding mustBe empty
          result.paymentOnAccount mustBe empty
          result.cleared mustBe empty
        }
      }

      "default dateFrom to the fixed VPD service start date and dateTo to today when dates not provided" in {
        when(mockConnector.getFinancialData(any(), any(), any())(using any()))
          .thenReturn(Future.successful(testResponse))

        whenReady(service.getPayments(testVpdId, None, None)) { _ =>
          val vpdIdCaptor = ArgumentCaptor.forClass(classOf[String])
          val dateFromCaptor = ArgumentCaptor.forClass(classOf[LocalDate])
          val dateToCaptor = ArgumentCaptor.forClass(classOf[LocalDate])
          verify(mockConnector, atLeastOnce()).getFinancialData(vpdIdCaptor.capture(), dateFromCaptor.capture(), dateToCaptor.capture())(using any())

          dateFromCaptor.getValue mustBe appConfig.financialDataStartDate
          dateToCaptor.getValue mustBe LocalDate.now(clock)
        }
      }

      "use provided dates when supplied" in {
        when(mockConnector.getFinancialData(any(), any(), any())(using any()))
          .thenReturn(Future.successful(testResponse))

        whenReady(service.getPayments(testVpdId, Some(LocalDate.of(2024, 1, 1)), Some(LocalDate.of(2024, 12, 31)))) { _ =>
          val vpdIdCaptor = ArgumentCaptor.forClass(classOf[String])
          val dateFromCaptor = ArgumentCaptor.forClass(classOf[LocalDate])
          val dateToCaptor = ArgumentCaptor.forClass(classOf[LocalDate])
          verify(mockConnector, atLeastOnce()).getFinancialData(vpdIdCaptor.capture(), dateFromCaptor.capture(), dateToCaptor.capture())(using any())

          dateFromCaptor.getValue mustBe LocalDate.of(2024, 1, 1)
          dateToCaptor.getValue mustBe LocalDate.of(2024, 12, 31)
        }
      }

      "propagate failure when connector fails" in {
        val exception = UpstreamErrorResponse("API error", INTERNAL_SERVER_ERROR)
        when(mockConnector.getFinancialData(any(), any(), any())(using any()))
          .thenReturn(Future.failed(exception))

        whenReady(service.getPayments(testVpdId, Some(LocalDate.of(2024, 1, 1)), Some(LocalDate.of(2024, 12, 31))).failed) { ex =>
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

        when(mockConnector.getFinancialData(any(), any(), any())(using any()))
          .thenReturn(Future.successful(response))

        whenReady(service.getPayments(testVpdId, Some(LocalDate.of(2024, 1, 1)), Some(LocalDate.of(2024, 12, 31)))) { result =>
          result.outstanding.head.status mustBe PaymentStatus.Due
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

        when(mockConnector.getFinancialData(any(), any(), any())(using any()))
          .thenReturn(Future.successful(response))

        whenReady(service.getPayments(testVpdId, Some(LocalDate.of(2024, 1, 1)), Some(LocalDate.of(2024, 12, 31)))) { result =>
          result.outstanding.head.status mustBe PaymentStatus.Overdue
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

        when(mockConnector.getFinancialData(any(), any(), any())(using any()))
          .thenReturn(Future.successful(response))

        whenReady(service.getPayments(testVpdId, Some(LocalDate.of(2024, 1, 1)), Some(LocalDate.of(2024, 12, 31)))) { result =>
          result.outstanding.head.status mustBe PaymentStatus.Due
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

        when(mockConnector.getFinancialData(any(), any(), any())(using any()))
          .thenReturn(Future.successful(response))

        whenReady(service.getPayments(testVpdId, Some(LocalDate.of(2024, 1, 1)), Some(LocalDate.of(2024, 12, 31)))) { result =>
          result.outstanding.head.amountDue mustBe preciseAmount
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

        when(mockConnector.getFinancialData(any(), any(), any())(using any()))
          .thenReturn(Future.successful(response))

        whenReady(service.getPayments(testVpdId, Some(LocalDate.of(2024, 1, 1)), Some(LocalDate.of(2024, 12, 31)))) { result =>
          result.outstanding.size mustBe 2
        }
      }

      "treat payment on account with VPD contract object as payment on account" in {
        val response = testResponse.copy(
          success = testResponse.success.copy(
            financialData = Some(FinancialData(
              totalisation = Some(sampleTotalisation),
              documentDetails = Some(Seq(testDocUnallocated))
            ))
          )
        )

        when(mockConnector.getFinancialData(any(), any(), any())(using any()))
          .thenReturn(Future.successful(response))

        whenReady(service.getPayments(testVpdId, Some(LocalDate.of(2024, 1, 1)), Some(LocalDate.of(2024, 12, 31)))) { result =>
          result.outstanding mustBe empty
          result.cleared mustBe empty
          result.paymentOnAccount mustBe empty
        }
      }

      "not treat payment on account with non-VPD contract object as payment on account" in {
        val nonVpdDoc = testDocUnallocated.copy(
          contractObjectType = Some("ZADP")
        )

        val response = testResponse.copy(
          success = testResponse.success.copy(
            financialData = Some(FinancialData(
              totalisation = Some(sampleTotalisation),
              documentDetails = Some(Seq(nonVpdDoc))
            ))
          )
        )

        when(mockConnector.getFinancialData(any(), any(), any())(using any()))
          .thenReturn(Future.successful(response))

        whenReady(service.getPayments(testVpdId, Some(LocalDate.of(2024, 1, 1)), Some(LocalDate.of(2024, 12, 31)))) { result =>
          result.outstanding mustBe empty
          result.cleared mustBe empty
          result.paymentOnAccount mustBe empty
        }
      }

      "not treat payment on account with missing contract object as payment on account" in {
        val noContractObjectDoc = testDocUnallocated.copy(
          contractObjectType = None
        )

        val response = testResponse.copy(
          success = testResponse.success.copy(
            financialData = Some(FinancialData(
              totalisation = Some(sampleTotalisation),
              documentDetails = Some(Seq(noContractObjectDoc))
            ))
          )
        )

        when(mockConnector.getFinancialData(any(), any(), any())(using any()))
          .thenReturn(Future.successful(response))

        whenReady(service.getPayments(testVpdId, Some(LocalDate.of(2024, 1, 1)), Some(LocalDate.of(2024, 12, 31)))) { result =>
          result.outstanding mustBe empty
          result.cleared mustBe empty
          result.paymentOnAccount mustBe empty
        }
      }

      "not treat non-payment-on-account with VPD contract object as payment on account" in {
        val nonPaymentOnAccountDoc = testDocWithOutstanding.copy(
          lineItemDetails = Some(Seq(
            testDocWithOutstanding.lineItemDetails.get.head.copy(
              mainTransaction = Some("4060")
            )
          ))
        )

        val response = testResponse.copy(
          success = testResponse.success.copy(
            financialData = Some(FinancialData(
              totalisation = None,
              documentDetails = Some(Seq(nonPaymentOnAccountDoc))
            ))
          )
        )

        when(mockConnector.getFinancialData(any(), any(), any())(using any()))
          .thenReturn(Future.successful(response))

        whenReady(service.getPayments(testVpdId, Some(LocalDate.of(2024, 1, 1)), Some(LocalDate.of(2024, 12, 31)))) { result =>
          result.outstanding must not be empty
          result.cleared mustBe empty
          result.paymentOnAccount mustBe empty
        }
      }
    }

  }
}
