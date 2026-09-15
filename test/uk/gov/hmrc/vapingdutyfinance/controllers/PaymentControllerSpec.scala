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

package uk.gov.hmrc.vapingdutyfinance.controllers

import org.mockito.ArgumentMatchers.{any, eq as eqTo}
import org.mockito.Mockito.when
import play.api.libs.json.Json
import play.api.test.Helpers.*
import uk.gov.hmrc.http.UpstreamErrorResponse
import uk.gov.hmrc.vapingdutyfinance.base.SpecBase
import uk.gov.hmrc.vapingdutyfinance.models.payments.PaymentOrigin
import uk.gov.hmrc.vapingdutyfinance.services.{FinancialDataService, PaymentService}

import scala.concurrent.Future

class PaymentControllerSpec extends SpecBase {

  val mockPaymentService: PaymentService = mock[PaymentService]
  val mockFinancialDataService: FinancialDataService = mock[FinancialDataService]

  val controller = new PaymentController(
    cc,
    fakeAuthorisedAction,
    mockFinancialDataService,
    mockPaymentService
  )

  "PaymentController" - {
    "startPayment must" - {
      "return 200 OK with StartPaymentResponse when the service returns success" in {
        when(mockPaymentService.startPayment(eqTo(testStartPaymentRequest), eqTo(PaymentOrigin.Vpd))(using any()))
          .thenReturn(Future.successful(testStartPaymentResponse))

        val request = fakeRequest.withBody(Json.toJson(testStartPaymentRequest))
        val result = controller.startPayment()(request)

        status(result) mustBe OK
        contentAsJson(result) mustBe Json.toJson(testStartPaymentResponse)
      }

      Seq(
        BAD_REQUEST,
        NOT_FOUND,
        UNPROCESSABLE_ENTITY,
        INTERNAL_SERVER_ERROR,
        SERVICE_UNAVAILABLE
      ).foreach { statusCode =>
        s"return $statusCode with a generic error message when the service fails with $statusCode" in {
          when(mockPaymentService.startPayment(eqTo(testStartPaymentRequest), eqTo(PaymentOrigin.Vpd))(using any()))
            .thenReturn(Future.failed(UpstreamErrorResponse("some upstream detail that must not leak", statusCode)))

          val request = fakeRequest.withBody(Json.toJson(testStartPaymentRequest))
          val result = controller.startPayment()(request)

          status(result) mustBe statusCode
          contentAsJson(result) mustBe Json.obj("error" -> "An error occurred while starting the payment")
        }
      }

      "return 400 BAD_REQUEST when request body is invalid" in {
        val invalidJson = Json.obj("invalid" -> "data")
        val request = fakeRequest.withBody(invalidJson)
        val result = controller.startPayment()(request)

        status(result) mustBe BAD_REQUEST
        contentAsJson(result) mustBe Json.obj("error" -> "Invalid request body")
      }
    }

    "startBtaPayment must" - {
      "return 200 OK with StartPaymentResponse when balance is positive and payment succeeds" in {
        when(mockFinancialDataService.getPayments(eqTo(testVpdId), eqTo(None), eqTo(None))(using any()))
          .thenReturn(Future.successful(testPaymentsResponse))

        when(mockPaymentService.startPayment(eqTo(testStartPaymentRequest), eqTo(PaymentOrigin.Bta))(using any()))
          .thenReturn(Future.successful(testStartPaymentResponse))

        val request = fakeRequest.withBody(Json.toJson(testStartPaymentRequest))
        val result = controller.startBtaPayment()(request)

        status(result) mustBe OK
        contentAsJson(result) mustBe Json.toJson(testStartPaymentResponse)
      }

      "return 400 BAD_REQUEST when totalAccountBalance is zero" in {
        val paymentsWithZeroBalance = testPaymentsResponse.copy(totalAccountBalance = Some(BigDecimal("0.0")))

        when(mockFinancialDataService.getPayments(eqTo(testVpdId), eqTo(None), eqTo(None))(using any()))
          .thenReturn(Future.successful(paymentsWithZeroBalance))

        val request = fakeRequest.withBody(Json.toJson(testStartPaymentRequest))
        val result = controller.startBtaPayment()(request)

        status(result) mustBe BAD_REQUEST
        contentAsJson(result) mustBe Json.obj("error" -> "No outstanding balance to pay")
      }

      "return 400 BAD_REQUEST when totalAccountBalance is None" in {
        val paymentsWithNoBalance = testPaymentsResponse.copy(totalAccountBalance = None)

        when(mockFinancialDataService.getPayments(eqTo(testVpdId), eqTo(None), eqTo(None))(using any()))
          .thenReturn(Future.successful(paymentsWithNoBalance))

        val request = fakeRequest.withBody(Json.toJson(testStartPaymentRequest))
        val result = controller.startBtaPayment()(request)

        status(result) mustBe BAD_REQUEST
        contentAsJson(result) mustBe Json.obj("error" -> "No outstanding balance to pay")
      }

      "return 400 BAD_REQUEST when totalAccountBalance is negative" in {
        val paymentsWithNegativeBalance = testPaymentsResponse.copy(totalAccountBalance = Some(BigDecimal("-50.0")))

        when(mockFinancialDataService.getPayments(eqTo(testVpdId), eqTo(None), eqTo(None))(using any()))
          .thenReturn(Future.successful(paymentsWithNegativeBalance))

        val request = fakeRequest.withBody(Json.toJson(testStartPaymentRequest))
        val result = controller.startBtaPayment()(request)

        status(result) mustBe BAD_REQUEST
        contentAsJson(result) mustBe Json.obj("error" -> "No outstanding balance to pay")
      }

      "return 400 BAD_REQUEST when request body is invalid" in {
        when(mockFinancialDataService.getPayments(eqTo(testVpdId), eqTo(None), eqTo(None))(using any()))
          .thenReturn(Future.successful(testPaymentsResponse))

        val invalidJson = Json.obj("invalid" -> "data")
        val request = fakeRequest.withBody(invalidJson)
        val result = controller.startBtaPayment()(request)

        status(result) mustBe BAD_REQUEST
        contentAsJson(result) mustBe Json.obj("error" -> "Invalid request body")
      }

      Seq(
        BAD_REQUEST,
        NOT_FOUND,
        UNPROCESSABLE_ENTITY,
        INTERNAL_SERVER_ERROR,
        SERVICE_UNAVAILABLE
      ).foreach { statusCode =>
        s"return $statusCode with a generic error message when the payment service fails with $statusCode" in {
          when(mockFinancialDataService.getPayments(eqTo(testVpdId), eqTo(None), eqTo(None))(using any()))
            .thenReturn(Future.successful(testPaymentsResponse))

          when(mockPaymentService.startPayment(eqTo(testStartPaymentRequest), eqTo(PaymentOrigin.Bta))(using any()))
            .thenReturn(Future.failed(UpstreamErrorResponse("some upstream detail that must not leak", statusCode)))

          val request = fakeRequest.withBody(Json.toJson(testStartPaymentRequest))
          val result = controller.startBtaPayment()(request)

          status(result) mustBe statusCode
          contentAsJson(result) mustBe Json.obj("error" -> "An error occurred while starting the payment")
        }
      }
    }
  }
}