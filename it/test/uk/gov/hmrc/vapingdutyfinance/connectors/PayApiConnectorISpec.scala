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

package uk.gov.hmrc.vapingdutyfinance.connectors

import play.api.http.Status.*
import play.api.libs.json.Json
import uk.gov.hmrc.http.UpstreamErrorResponse
import uk.gov.hmrc.vapingdutyfinance.base.{ConnectorTestHelpers, SpecBase}
import uk.gov.hmrc.vapingdutyfinance.config.AppConfig

class PayApiConnectorISpec extends SpecBase with ConnectorTestHelpers {

  protected val endpointName = "pay-api"

  "PayApiConnector must" - {

    "return a StartPaymentResponse when pay-api returns 201 CREATED" in new SetUp {
      val responseBody: String = Json.toJson(testStartPaymentResponse).toString

      stubPost(path, CREATED, responseBody)

      whenReady(connector.startPayment(testStartPaymentRequest)) { result =>
        result mustBe testStartPaymentResponse
        verifyPost(path)
      }
    }

    "fail with UpstreamErrorResponse when pay-api returns 201 with invalid JSON" in new SetUp {
      val invalidResponseBody = """{"invalid": "json"}"""

      stubPost(path, CREATED, invalidResponseBody)

      whenReady(connector.startPayment(testStartPaymentRequest).failed) { exception =>
        exception mustBe an[UpstreamErrorResponse]
        val upstreamError = exception.asInstanceOf[UpstreamErrorResponse]
        upstreamError.statusCode mustBe INTERNAL_SERVER_ERROR
        upstreamError.message mustBe "Invalid JSON response from pay-api"
        verifyPost(path)
      }
    }

    "fail with UpstreamErrorResponse on 200 OK instead of 201 Created" in new SetUp {
      stubPost(path, OK, Json.toJson(testStartPaymentResponse).toString())

      whenReady(connector.startPayment(testStartPaymentRequest).failed) { exception =>
        exception mustBe an[UpstreamErrorResponse]
        val upstreamError = exception.asInstanceOf[UpstreamErrorResponse]
        upstreamError.statusCode mustBe OK
        upstreamError.message mustBe "Unexpected response from pay-api"
        verifyPost(path)
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
      s"fail with UpstreamErrorResponse when pay-api returns $statusCode" in new SetUp {
        stubPost(path, statusCode, "")

        whenReady(connector.startPayment(testStartPaymentRequest).failed) { exception =>
          exception mustBe an[UpstreamErrorResponse]
          val upstreamError = exception.asInstanceOf[UpstreamErrorResponse]
          upstreamError.statusCode mustBe statusCode
          upstreamError.message mustBe "Unexpected response from pay-api"
          verifyPost(path)
        }
      }
    }

    "fail on network fault" in new SetUp {
      stubPostFault(path)

      whenReady(connector.startPayment(testStartPaymentRequest).failed) { exception =>
        exception mustBe a[Exception]
        verifyPost(path)
      }
    }
  }

  class SetUp extends ConnectorFixture {
    val connector: PayApiConnector = appWithHttpClient.injector.instanceOf[PayApiConnector]
    lazy val url: String = appWithHttpClient.injector.instanceOf[AppConfig].payApiUrl
    lazy val path = new java.net.URL(url).getPath
  }
}
