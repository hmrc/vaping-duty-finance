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
import uk.gov.hmrc.vapingdutyfinance.models.financialdata.*
import uk.gov.hmrc.vapingdutyfinance.utils.UUIDGenerator

import java.time.format.DateTimeFormatter
import java.time.{Clock, Instant, LocalDate, ZoneOffset}
import javax.inject.{Inject, Singleton}
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Success, Try}

@Singleton
class FinancialDataConnector @Inject()(
                                        httpClient: HttpClientV2,
                                        clock: Clock,
                                        appConfig: AppConfig,
                                        uuidGenerator: UUIDGenerator
                                      )(using ExecutionContext) extends Logging with HttpReadsInstances {

  private val dateTimeFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss'Z'")
  private val noDataIdentifiedErrorCode = "018"

  private def buildRequest(vpdId: String, dateFrom: LocalDate, dateTo: LocalDate): FinancialDataRequest = {
    FinancialDataRequest(
      taxRegime = appConfig.taxRegimeVpd,
      taxpayerInformation = TaxpayerInformation(
        idType = appConfig.idTypeVpd,
        idNumber = vpdId
      ),
      selectionCriteria = SelectionCriteria(
        dateRange = DateRange(
          dateType = appConfig.dateTypePosting,
          dateFrom = dateFrom,
          dateTo = dateTo
        ),
        includeClearedItems = true,
        includeStatisticalItems = false,
        includePaymentOnAccount = false
      ),
      dataEnrichment = DataEnrichmentOptions(
        addRegimeTotalisation = true,
        addLockInformation = false,
        addPenaltyDetails = true,
        addPostedInterestDetails = false,
        addAccruingInterestDetails = true
      )
    )
  }

  def getFinancialData(vpdId: String, dateFrom: LocalDate, dateTo: LocalDate)
                      (using hc: HeaderCarrier): Future[FinancialDataResponse] = {

    val request = buildRequest(vpdId, dateFrom, dateTo)
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
      .recoverWith { case e: Exception =>
        logger.warn(s"Error calling financial data API: ${e.getMessage}", e)
        Future.failed(e)
      }
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
          case UNPROCESSABLE_ENTITY =>
            Try(Json.parse(response.body).validate[FinancialDataErrorResponse]) match {
              case Success(JsSuccess(FinancialDataErrorResponse(FinancialDataError(_, code, _)), _)) if code == noDataIdentifiedErrorCode =>
                logger.info("Financial data API returned code 018 (No Data Identified) - treating as an empty result")
                Future.successful(FinancialDataResponse(
                  success = FinancialDataSuccess(
                    processingDate = Instant.now(clock),
                    financialData = None
                  )
                ))
              case Success(JsSuccess(errorResponse, _)) =>
                logger.warn(s"Financial data API returned business error: code=${errorResponse.errors.code} text=${errorResponse.errors.text}")
                Future.failed(UpstreamErrorResponse(s"Financial data API returned 422 business error ${errorResponse.errors.code}", UNPROCESSABLE_ENTITY))
              case _ =>
                logger.warn("Failed to parse 422 error response from financial data API")
                Future.failed(UpstreamErrorResponse("Unexpected response from financial data API", UNPROCESSABLE_ENTITY))
            }
          case status =>
            logger.warn(s"Unexpected response from financial data API: status=$status")
            Future.failed(UpstreamErrorResponse("Unexpected response from financial data API", status))
        }
      }
  }
}
