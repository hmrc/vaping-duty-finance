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
import uk.gov.hmrc.vapingdutyfinance.models.directdebit.DirectDebitOrigin
import uk.gov.hmrc.vapingdutyfinance.services.DirectDebitService

import scala.concurrent.Future

class DirectDebitControllerSpec extends SpecBase {

  val mockDirectDebitService: DirectDebitService = mock[DirectDebitService]

  val controller = new DirectDebitController(
    cc,
    fakeAuthorisedAction,
    mockDirectDebitService
  )

  "DirectDebitController" - {
    "startVpdConfirmation must" - {
      "return 200 OK with StartDirectDebitResponse when the direct debit service starts a journey" in {
        when(mockDirectDebitService.startDirectDebit(eqTo(startDirectDebitRequest), eqTo(DirectDebitOrigin.VpdConfirmation))(using any()))
          .thenReturn(Future.successful(startDirectDebitResponse))

        val result = controller.startVpdConfirmation()(fakeRequest.withBody(Json.toJson(startDirectDebitRequest)))

        status(result) mustBe OK
        contentAsJson(result) mustBe Json.toJson(startDirectDebitResponse)
      }

      "return 400 BAD_REQUEST when the request body is invalid" in {
        val invalidJson = Json.obj("invalid" -> "data")

        val result = controller.startVpdConfirmation()(fakeRequest.withBody(invalidJson))

        status(result) mustBe BAD_REQUEST
        contentAsJson(result) mustBe Json.obj("error" -> "Invalid request body")
      }

      "return 500 INTERNAL_SERVER_ERROR with a generic error message when the service fails" in {
        when(mockDirectDebitService.startDirectDebit(any(), any())(using any()))
          .thenReturn(Future.failed(new RuntimeException("some internal detail that must not leak")))

        val result = controller.startVpdConfirmation()(fakeRequest.withBody(Json.toJson(startDirectDebitRequest)))

        status(result) mustBe INTERNAL_SERVER_ERROR
        contentAsJson(result) mustBe Json.obj("error" -> "An error occurred while starting the direct debit journey")
      }
    }

    "startBta must" - {
      "return 200 OK with StartDirectDebitResponse when the direct debit service starts a journey" in {
        when(mockDirectDebitService.startDirectDebit(eqTo(startDirectDebitRequest), eqTo(DirectDebitOrigin.Bta))(using any()))
          .thenReturn(Future.successful(startDirectDebitResponse))

        val result = controller.startBta()(fakeRequest.withBody(Json.toJson(startDirectDebitRequest)))

        status(result) mustBe OK
        contentAsJson(result) mustBe Json.toJson(startDirectDebitResponse)
      }

      "return 400 BAD_REQUEST when the request body is invalid" in {
        val invalidJson = Json.obj("invalid" -> "data")

        val result = controller.startBta()(fakeRequest.withBody(invalidJson))

        status(result) mustBe BAD_REQUEST
        contentAsJson(result) mustBe Json.obj("error" -> "Invalid request body")
      }

      "return 500 INTERNAL_SERVER_ERROR with a generic error message when the service fails" in {
        when(mockDirectDebitService.startDirectDebit(any(), any())(using any()))
          .thenReturn(Future.failed(new RuntimeException("some internal detail that must not leak")))

        val result = controller.startBta()(fakeRequest.withBody(Json.toJson(startDirectDebitRequest)))

        status(result) mustBe INTERNAL_SERVER_ERROR
        contentAsJson(result) mustBe Json.obj("error" -> "An error occurred while starting the direct debit journey")
      }
    }
  }
}
