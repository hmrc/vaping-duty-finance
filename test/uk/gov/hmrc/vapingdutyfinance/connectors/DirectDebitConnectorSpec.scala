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

package uk.gov.hmrc.vapingdutyfinance.connectors

import org.mockito.ArgumentMatchers.any
import org.mockito.Mockito.when
import play.api.http.Status.*
import play.api.libs.json.Json
import uk.gov.hmrc.http.client.{HttpClientV2, RequestBuilder}
import uk.gov.hmrc.http.{HttpResponse, UpstreamErrorResponse}
import uk.gov.hmrc.vapingdutyfinance.base.SpecBase
import uk.gov.hmrc.vapingdutyfinance.models.directdebit.DirectDebitOrigin

import scala.concurrent.Future

class DirectDebitConnectorSpec extends SpecBase {

  val mockHttpClient: HttpClientV2 = mock[HttpClientV2]
  val mockRequestBuilder: RequestBuilder = mock[RequestBuilder]

  val connector = new DirectDebitConnector(mockHttpClient, appConfig)

  private def stubRequestBuilderChain(response: Future[HttpResponse]): Unit = {
    when(mockHttpClient.post(any())(any())).thenReturn(mockRequestBuilder)
    when(mockRequestBuilder.setHeader(any())).thenReturn(mockRequestBuilder)
    when(mockRequestBuilder.withBody(any())(any(), any(), any())).thenReturn(mockRequestBuilder)
    when(mockRequestBuilder.execute[HttpResponse](any(), any())).thenReturn(response)
  }

  "DirectDebitConnector must" - {
    "return a StartDirectDebitResponse when direct-debit-backend returns 201 CREATED" in {
      val responseBody = Json.toJson(startDirectDebitResponse).toString
      stubRequestBuilderChain(Future.successful(HttpResponse(CREATED, responseBody)))

      whenReady(connector.startDirectDebit(startDirectDebitRequest, DirectDebitOrigin.VpdConfirmation)) { result =>
        result mustBe startDirectDebitResponse
      }
    }

    "fail with UpstreamErrorResponse when direct-debit-backend returns 201 with invalid JSON" in {
      val invalidResponseBody = """{"invalid": "json"}"""
      stubRequestBuilderChain(Future.successful(HttpResponse(CREATED, invalidResponseBody)))

      whenReady(connector.startDirectDebit(startDirectDebitRequest, DirectDebitOrigin.VpdConfirmation).failed) { exception =>
        exception mustBe an[UpstreamErrorResponse]
        val upstreamError = exception.asInstanceOf[UpstreamErrorResponse]
        upstreamError.statusCode mustBe INTERNAL_SERVER_ERROR
        upstreamError.message mustBe "Invalid JSON response from direct-debit-backend"
      }
    }

    Seq(
      ("BadRequest", BAD_REQUEST),
      ("Unauthorized", UNAUTHORIZED),
      ("Forbidden", FORBIDDEN),
      ("NotFound", NOT_FOUND),
      ("UnprocessableEntity", UNPROCESSABLE_ENTITY),
      ("InternalServerError", INTERNAL_SERVER_ERROR),
      ("ServiceUnavailable", SERVICE_UNAVAILABLE)
    ).foreach { case (errorName, statusCode) =>
      s"fail with UpstreamErrorResponse when direct-debit-backend returns $statusCode" in {
        stubRequestBuilderChain(Future.successful(HttpResponse(statusCode, "")))

        whenReady(connector.startDirectDebit(startDirectDebitRequest, DirectDebitOrigin.VpdConfirmation).failed) { exception =>
          exception mustBe an[UpstreamErrorResponse]
          val upstreamError = exception.asInstanceOf[UpstreamErrorResponse]
          upstreamError.statusCode mustBe statusCode
          upstreamError.message mustBe "Unexpected response from direct-debit-backend"
        }
      }
    }

    "fail on network fault" in {
      stubRequestBuilderChain(Future.failed(new RuntimeException("Network error")))

      whenReady(connector.startDirectDebit(startDirectDebitRequest, DirectDebitOrigin.VpdConfirmation).failed) { exception =>
        exception mustBe a[RuntimeException]
      }
    }
  }
}
