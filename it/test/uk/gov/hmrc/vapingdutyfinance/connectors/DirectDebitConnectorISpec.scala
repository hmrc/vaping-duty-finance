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

import play.api.http.Status.*
import play.api.libs.json.Json
import uk.gov.hmrc.http.UpstreamErrorResponse
import uk.gov.hmrc.vapingdutyfinance.base.{ConnectorTestHelpers, SpecBase}
import uk.gov.hmrc.vapingdutyfinance.config.AppConfig
import uk.gov.hmrc.vapingdutyfinance.models.directdebit.DirectDebitOrigin

class DirectDebitConnectorISpec extends SpecBase with ConnectorTestHelpers {

  protected val endpointName = "direct-debit"

  "DirectDebitConnector must" - {

    "return a StartDirectDebitResponse when direct-debit-backend returns 201 CREATED for VpdConfirmation" in new SetUp {
      val responseBody: String = Json.toJson(testStartDirectDebitResponse).toString

      stubPost(vpdConfirmationPath, CREATED, responseBody)

      whenReady(connector.startDirectDebit(testStartDirectDebitRequest, DirectDebitOrigin.VpdConfirmation)) { result =>
        result mustBe testStartDirectDebitResponse
        verifyPost(vpdConfirmationPath)
      }
    }

    "return a StartDirectDebitResponse when direct-debit-backend returns 201 CREATED for Bta" in new SetUp {
      val responseBody: String = Json.toJson(testStartDirectDebitResponse).toString

      stubPost(btaPath, CREATED, responseBody)

      whenReady(connector.startDirectDebit(testStartDirectDebitRequest, DirectDebitOrigin.Bta)) { result =>
        result mustBe testStartDirectDebitResponse
        verifyPost(btaPath)
      }
    }

    "fail with UpstreamErrorResponse when direct-debit-backend returns 201 with invalid JSON" in new SetUp {
      val invalidResponseBody = """{"invalid": "json"}"""

      stubPost(vpdConfirmationPath, CREATED, invalidResponseBody)

      whenReady(connector.startDirectDebit(testStartDirectDebitRequest, DirectDebitOrigin.VpdConfirmation).failed) { exception =>
        exception mustBe an[UpstreamErrorResponse]
        val upstreamError = exception.asInstanceOf[UpstreamErrorResponse]
        upstreamError.statusCode mustBe INTERNAL_SERVER_ERROR
        upstreamError.message mustBe "Invalid JSON response from direct-debit-backend"
        verifyPost(vpdConfirmationPath)
      }
    }

    "fail with UpstreamErrorResponse on 200 OK instead of 201 Created" in new SetUp {
      stubPost(vpdConfirmationPath, OK, Json.toJson(testStartDirectDebitResponse).toString())

      whenReady(connector.startDirectDebit(testStartDirectDebitRequest, DirectDebitOrigin.VpdConfirmation).failed) { exception =>
        exception mustBe an[UpstreamErrorResponse]
        val upstreamError = exception.asInstanceOf[UpstreamErrorResponse]
        upstreamError.statusCode mustBe OK
        upstreamError.message mustBe "Unexpected response from direct-debit-backend"
        verifyPost(vpdConfirmationPath)
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
      s"fail with UpstreamErrorResponse when direct-debit-backend returns $statusCode" in new SetUp {
        stubPost(vpdConfirmationPath, statusCode, "")

        whenReady(connector.startDirectDebit(testStartDirectDebitRequest, DirectDebitOrigin.VpdConfirmation).failed) { exception =>
          exception mustBe an[UpstreamErrorResponse]
          val upstreamError = exception.asInstanceOf[UpstreamErrorResponse]
          upstreamError.statusCode mustBe statusCode
          upstreamError.message mustBe "Unexpected response from direct-debit-backend"
          verifyPost(vpdConfirmationPath)
        }
      }
    }

    "fail on network fault" in new SetUp {
      stubPostFault(vpdConfirmationPath)

      whenReady(connector.startDirectDebit(testStartDirectDebitRequest, DirectDebitOrigin.VpdConfirmation).failed) { exception =>
        exception mustBe a[Exception]
        verifyPost(vpdConfirmationPath)
      }
    }
  }

  class SetUp extends ConnectorFixture {
    val connector: DirectDebitConnector = appWithHttpClient.injector.instanceOf[DirectDebitConnector]
    lazy val vpdConfirmationUrl: String = appWithHttpClient.injector.instanceOf[AppConfig].directDebitVpdConfirmationUrl
    lazy val btaUrl: String = appWithHttpClient.injector.instanceOf[AppConfig].directDebitBtaUrl
    lazy val vpdConfirmationPath = new java.net.URL(vpdConfirmationUrl).getPath
    lazy val btaPath = new java.net.URL(btaUrl).getPath
  }
}
