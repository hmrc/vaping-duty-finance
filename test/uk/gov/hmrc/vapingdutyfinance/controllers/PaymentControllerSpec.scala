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
import uk.gov.hmrc.vapingdutyfinance.services.PaymentService

import scala.concurrent.Future

class PaymentControllerSpec extends SpecBase {

  val mockPaymentService: PaymentService = mock[PaymentService]

  val controller = new PaymentController(
    cc,
    fakeAuthorisedAction,
    mockPaymentService
  )

  "PaymentController" - {
    "startPayment must" - {
      "return 200 OK with StartPaymentResponse when the service returns success" in {
        when(mockPaymentService.startPayment(eqTo(testStartPaymentRequest))(using any()))
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
          when(mockPaymentService.startPayment(eqTo(testStartPaymentRequest))(using any()))
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
  }
}
