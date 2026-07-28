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
import play.api.http.Status
import play.api.libs.json.Json
import play.api.test.FakeRequest
import play.api.test.Helpers.*
import uk.gov.hmrc.vapingdutyfinance.base.SpecBase
import uk.gov.hmrc.vapingdutyfinance.models.PaymentsResponse
import uk.gov.hmrc.vapingdutyfinance.services.FinancialDataService

import scala.concurrent.Future

class FinancialDataControllerSpec extends SpecBase {

  val mockService: FinancialDataService = mock[FinancialDataService]

  val controller = FinancialDataController(
    fakeAuthorisedAction,
    mockService,
    cc
  )

  "FinancialDataController" - {
    "getPayments must" - {
      "return 200 OK with all three payment sections when service returns success" in {
        when(mockService.getPayments(eqTo(testVpdId), any(), any())(using any()))
          .thenReturn(Future.successful(testPaymentsResponse))

        val request = FakeRequest(GET, routes.FinancialDataController.getPayments(None, None).url)
        val result = controller.getPayments(None, None)(request)

        status(result) mustBe Status.OK
        contentAsJson(result) mustBe Json.toJson(testPaymentsResponse)
      }

      "return 200 OK with empty sections when no payments exist" in {
        when(mockService.getPayments(eqTo(testVpdId), any(), any())(using any()))
          .thenReturn(Future.successful(PaymentsResponse.empty))

        val request = FakeRequest(GET, routes.FinancialDataController.getPayments(None, None).url)
        val result = controller.getPayments(None, None)(request)

        status(result) mustBe Status.OK
        contentAsJson(result) mustBe Json.toJson(PaymentsResponse.empty)
      }

      "return 200 OK when valid date parameters are provided" in {
        when(mockService.getPayments(eqTo(testVpdId), any(), any())(using any()))
          .thenReturn(Future.successful(testPaymentsResponse))

        val request =
          FakeRequest(GET, routes.FinancialDataController.getPayments(Some("2024-01-01"), Some("2024-12-31")).url)

        val result = controller.getPayments(Some("2024-01-01"), Some("2024-12-31"))(request)

        status(result) mustBe Status.OK
        contentAsJson(result) mustBe Json.toJson(testPaymentsResponse)
      }

      "return 500 Internal Server Error when service fails" in {
        when(mockService.getPayments(eqTo(testVpdId), any(), any())(using any()))
          .thenReturn(Future.failed(new RuntimeException("Service error")))

        val request = FakeRequest(GET, routes.FinancialDataController.getPayments(None, None).url)
        val result = controller.getPayments(None, None)(request)

        status(result) mustBe Status.INTERNAL_SERVER_ERROR
        (contentAsJson(result) \ "error").as[String] mustBe "An error occurred while retrieving financial data"
      }

      "handle invalid date format gracefully" in {
        when(mockService.getPayments(eqTo(testVpdId), any(), any())(using any()))
          .thenReturn(Future.successful(testPaymentsResponse))

        val request =
          FakeRequest(GET, routes.FinancialDataController.getPayments(Some("invalid-date"), Some("2024-12-31")).url)

        val result = controller.getPayments(Some("invalid-date"), Some("2024-12-31"))(request)

        status(result) mustBe Status.OK
      }
    }
  }
}
