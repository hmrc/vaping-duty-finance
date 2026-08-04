/*
 * Copyright 2026 HM Revenue & Customs
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

package uk.gov.hmrc.vapingdutyfinance.controllers

import org.mockito.ArgumentMatchers.{any, eq as eqTo}
import org.mockito.Mockito.when
import play.api.libs.json.Json
import play.api.test.Helpers.*
import uk.gov.hmrc.vapingdutyfinance.base.SpecBase
import uk.gov.hmrc.vapingdutyfinance.models.PaymentsResponse
import uk.gov.hmrc.vapingdutyfinance.services.{FinancialDataService, PaymentService}

import scala.concurrent.Future

class PaymentRedirectControllerSpec extends SpecBase {

  val mockFinancialDataService: FinancialDataService = mock[FinancialDataService]
  val mockPaymentService: PaymentService             = mock[PaymentService]

  val controller = new PaymentRedirectController(
    cc,
    fakeAuthorisedAction,
    mockFinancialDataService,
    mockPaymentService
  )

  "PaymentRedirectController" - {
    "pay must" - {
      "redirect to nextUrl when the payment service starts a payment" in {
        when(mockFinancialDataService.getPayments(eqTo(testVpdId), eqTo(None), eqTo(None))(using any()))
          .thenReturn(Future.successful(PaymentsResponse(Seq.empty, Seq.empty, Seq.empty, Some(BigDecimal(82.50)))))
        when(mockPaymentService.startBtaPayment(eqTo(testVpdId), eqTo(BigDecimal(82.50)))(using any()))
          .thenReturn(Future.successful(testStartPaymentResponse))

        val result = controller.pay()(fakeRequest)

        status(result) mustBe SEE_OTHER
        redirectLocation(result) mustBe Some(testNextUrl)
      }

      Seq(
        ("no balance is found", None),
        ("the balance is zero", Some(BigDecimal(0))),
        ("the balance is negative", Some(BigDecimal(-10)))
      ).foreach { case (scenario, totalAccountBalance) =>
        s"return 400 BAD_REQUEST without calling the payment service when $scenario" in {
          when(mockFinancialDataService.getPayments(eqTo(testVpdId), eqTo(None), eqTo(None))(using any()))
            .thenReturn(Future.successful(PaymentsResponse(Seq.empty, Seq.empty, Seq.empty, totalAccountBalance)))

          val result = controller.pay()(fakeRequest)

          status(result) mustBe BAD_REQUEST
          contentAsJson(result) mustBe Json.obj("error" -> "No outstanding balance to pay")
        }
      }

      "return 500 with a generic error message when the payment service fails" in {
        when(mockFinancialDataService.getPayments(eqTo(testVpdId), eqTo(None), eqTo(None))(using any()))
          .thenReturn(Future.successful(PaymentsResponse(Seq.empty, Seq.empty, Seq.empty, Some(BigDecimal(45.74)))))
        when(mockPaymentService.startBtaPayment(any(), any())(using any()))
          .thenReturn(Future.failed(new RuntimeException("some internal detail that must not leak")))

        val result = controller.pay()(fakeRequest)

        status(result) mustBe INTERNAL_SERVER_ERROR
        contentAsJson(result) mustBe Json.obj("error" -> "An error occurred while starting the payment")
      }
    }
  }
}
