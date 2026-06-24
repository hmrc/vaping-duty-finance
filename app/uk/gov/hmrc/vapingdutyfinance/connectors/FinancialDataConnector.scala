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
import uk.gov.hmrc.http.{HeaderCarrier, HttpResponse, StringContextOps, UpstreamErrorResponse}
import uk.gov.hmrc.vapingdutyfinance.config.AppConfig
import uk.gov.hmrc.vapingdutyfinance.models.financialdata.{FinancialDataRequest, FinancialDataResponse}
import uk.gov.hmrc.vapingdutyfinance.utils.UUIDGenerator

import java.time.format.DateTimeFormatter
import java.time.{Clock, Instant, ZoneOffset}
import javax.inject.{Inject, Singleton}
import scala.concurrent.{ExecutionContext, Future}

@Singleton
class FinancialDataConnector @Inject()(
                                        httpClient: HttpClientV2,
                                        clock: Clock,
                                        appConfig: AppConfig,
                                        uuidGenerator: UUIDGenerator
                                      )(using ExecutionContext) extends Logging {

  private val dateTimeFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss'Z'")

  def getFinancialData(request: FinancialDataRequest)
                      (using hc: HeaderCarrier): Future[FinancialDataResponse] = {

    val correlationId = hc.requestId.map(_.value).getOrElse(uuidGenerator.uuid)
    val receiptDate = Instant.now(clock).atOffset(ZoneOffset.UTC).format(dateTimeFormatter)

    httpClient
      .post(url"${appConfig.financialDataUrl}")
      .setHeader("correlationid" -> correlationId)
      .setHeader("X-Originating-System" -> appConfig.originatingSystemVpd)
      .setHeader("X-Receipt-Date" -> receiptDate)
      .setHeader("X-Transmitting-System" -> appConfig.transmittingSystem)
      .setHeader("Content-Type" -> "application/json")
      .withBody(Json.toJson(request))
      .execute[HttpResponse]
      .flatMap { response =>
        response.status match {
          case CREATED =>
            Json.parse(response.body).validate[FinancialDataResponse] match {
              case JsSuccess(data, _) =>
                Future.successful(data)
              case JsError(errors) =>
                logger.warn(s"Failed to parse financial data response: $errors")
                Future.failed(UpstreamErrorResponse("Invalid JSON response from financial data API", INTERNAL_SERVER_ERROR))
            }
          case status =>
            logger.warn(s"Unexpected response from financial data API: status=$status")
            Future.failed(UpstreamErrorResponse("Unexpected response from financial data API", status))
        }
      }
  }
}
