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

import play.api.Logging
import play.api.http.Status.*
import play.api.libs.json.{JsError, JsSuccess, Json}
import play.api.libs.ws.JsonBodyWritables.writeableOf_JsValue
import uk.gov.hmrc.http.client.HttpClientV2
import uk.gov.hmrc.http.{HeaderCarrier, HttpReadsInstances, HttpResponse, StringContextOps, UpstreamErrorResponse}
import uk.gov.hmrc.vapingdutyfinance.config.AppConfig
import uk.gov.hmrc.vapingdutyfinance.models.payments.{StartPaymentRequest, StartPaymentResponse}

import javax.inject.{Inject, Singleton}
import scala.concurrent.{ExecutionContext, Future}

@Singleton
class PayApiConnector @Inject()(
  httpClient: HttpClientV2,
  appConfig: AppConfig
)(using ExecutionContext) extends Logging with HttpReadsInstances {

  private val unexpectedResponseMessage = "Unexpected response from pay-api"
  private val invalidJsonMessage = "Invalid JSON response from pay-api"

  def startPayment(request: StartPaymentRequest)
                  (using hc: HeaderCarrier): Future[StartPaymentResponse] = {

    httpClient
      .post(url"${appConfig.payApiUrl}")
      .setHeader("Content-Type" -> "application/json")
      .withBody(Json.toJson(request))
      .execute[HttpResponse]
      .recoverWith { case e: Exception =>
        logger.warn(s"Error calling pay-api: ${e.getMessage}", e)
        Future.failed(e)
      }
      .flatMap { response =>
        response.status match {
          case CREATED =>
            Json.parse(response.body).validate[StartPaymentResponse] match {
              case JsSuccess(data, _) =>
                Future.successful(data)
              case JsError(errors) =>
                logger.warn(s"Failed to parse pay-api response: $errors")
                Future.failed(UpstreamErrorResponse(invalidJsonMessage, INTERNAL_SERVER_ERROR))
            }
          case status =>
            logger.warn(s"Unexpected response from pay-api: status=$status")
            Future.failed(UpstreamErrorResponse(unexpectedResponseMessage, status))
        }
      }
  }
}