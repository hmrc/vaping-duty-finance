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

import play.api.Logging
import play.api.libs.json.{JsValue, Json}
import play.api.mvc.{Action, ControllerComponents, Result}
import uk.gov.hmrc.http.UpstreamErrorResponse
import uk.gov.hmrc.play.bootstrap.backend.controller.BackendController
import uk.gov.hmrc.vapingdutyfinance.controllers.actions.AuthorisedAction
import uk.gov.hmrc.vapingdutyfinance.models.payments.{PaymentOrigin, StartPaymentRequest}
import uk.gov.hmrc.vapingdutyfinance.models.requests.IdentifierRequest
import uk.gov.hmrc.vapingdutyfinance.services.{FinancialDataService, PaymentService}

import javax.inject.{Inject, Singleton}
import scala.concurrent.{ExecutionContext, Future}

@Singleton
class PaymentController @Inject()(
                                   cc: ControllerComponents,
                                   authorisedAction: AuthorisedAction,
                                   financialDataService: FinancialDataService,
                                   paymentService: PaymentService
                                 )(using ExecutionContext) extends BackendController(cc) with Logging {

  private val invalidRequestMessage = "Invalid request body"
  private val paymentErrorMessage = "An error occurred while starting the payment"
  private val noPaymentDueMessage = "No outstanding balance to pay"


  def startPayment(): Action[JsValue] = startPaymentJourney(PaymentOrigin.Vpd)

  def startBtaPayment(): Action[JsValue] = authorisedAction.async(parse.json) { implicit request =>
    checkPositiveBalance(request).flatMap {
      case Some(errorResult) => Future.successful(errorResult)
      case None => validateRequest(request.body, PaymentOrigin.Bta, " for BTA")
    }
  }

  private def startPaymentJourney(origin: PaymentOrigin): Action[JsValue] = {
    authorisedAction.async(parse.json) { implicit request =>
      validateRequest(request.body, origin, "")
    }
  }

  private def checkPositiveBalance(implicit request: IdentifierRequest[JsValue]): Future[Option[Result]] = {
    financialDataService.getPayments(request.vpdId, dateFrom = None, dateTo = None).map { payments =>
      payments.totalAccountBalance.filter(_ > 0) match {
        case Some(_) => None
        case None =>
          logger.warn(s"No positive totalAccountBalance found for vpdId=${request.vpdId}")
          Some(BadRequest(Json.obj("error" -> noPaymentDueMessage)))
      }
    }
  }

  private def validateRequest(
                               body: JsValue,
                               origin: PaymentOrigin,
                               logSuffix: String
                             )(implicit request: IdentifierRequest[?]): Future[Result] = {
    body.validate[StartPaymentRequest].fold(
      errors => {
        logger.warn(s"Invalid StartPaymentRequest$logSuffix: $errors")
        Future.successful(BadRequest(Json.obj("error" -> invalidRequestMessage)))
      },
      paymentRequest => startPaymentJourney(paymentRequest, origin)
    )
  }

  private def startPaymentJourney(
                                   paymentRequest: StartPaymentRequest,
                                   origin: PaymentOrigin
                                 )(using request: IdentifierRequest[?]): Future[Result] = {

    paymentService.startPayment(paymentRequest, origin)
      .map(response => Ok(Json.toJson(response)))
      .recover {
        case e: UpstreamErrorResponse =>
          logger.error(s"Failed to start payment for vpdId=${request.vpdId}, origin=$origin: ${e.getMessage}", e)
          Status(e.statusCode)(Json.obj("error" -> paymentErrorMessage))
      }
  }
}
