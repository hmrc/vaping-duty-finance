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
import uk.gov.hmrc.vapingdutyfinance.base.SpecBase
import uk.gov.hmrc.vapingdutyfinance.connectors.PayApiConnector
import uk.gov.hmrc.vapingdutyfinance.models.payments.PaymentErrorResponse

import scala.concurrent.Future

class PaymentControllerSpec extends SpecBase {

  val mockConnector: PayApiConnector = mock[PayApiConnector]

  val controller = new PaymentController(
    cc,
    fakeAuthorisedAction,
    mockConnector
  )

  "PaymentController" - {
    "startPayment must" - {
      "return 200 OK with StartPaymentResponse when connector returns success" in {
        when(mockConnector.startPayment(eqTo(testStartPaymentRequest))(using any()))
          .thenReturn(Future.successful(Right(testStartPaymentResponse)))

        val request = fakeRequest.withBody(Json.toJson(testStartPaymentRequest))
        val result = controller.startPayment()(request)

        status(result) mustBe OK
        contentAsJson(result) mustBe Json.toJson(testStartPaymentResponse)
      }

      Seq(
        (BAD_REQUEST, "Bad request"),
        (NOT_FOUND, "Not found"),
        (UNPROCESSABLE_ENTITY, "Unprocessable entity"),
        (INTERNAL_SERVER_ERROR, "Internal server error"),
        (SERVICE_UNAVAILABLE, "Service unavailable")
      ).foreach { case (statusCode, message) =>
        s"return $statusCode when connector returns $statusCode" in {
          val errorResponse = PaymentErrorResponse(statusCode, message)

          when(mockConnector.startPayment(eqTo(testStartPaymentRequest))(using any()))
            .thenReturn(Future.successful(Left(errorResponse)))

          val request = fakeRequest.withBody(Json.toJson(testStartPaymentRequest))
          val result = controller.startPayment()(request)

          status(result) mustBe statusCode
          contentAsJson(result) mustBe Json.toJson(errorResponse)
        }
      }

      "return 400 BAD_REQUEST when request body is invalid" in {
        val invalidJson = Json.obj("invalid" -> "data")
        val request = fakeRequest.withBody(invalidJson)
        val result = controller.startPayment()(request)

        status(result) mustBe BAD_REQUEST
      }
    }
  }
}