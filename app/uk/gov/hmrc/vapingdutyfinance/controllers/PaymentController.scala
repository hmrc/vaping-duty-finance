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
import play.api.mvc.{Action, ControllerComponents}
import uk.gov.hmrc.http.UpstreamErrorResponse
import uk.gov.hmrc.play.bootstrap.backend.controller.BackendController
import uk.gov.hmrc.vapingdutyfinance.controllers.actions.AuthorisedAction
import uk.gov.hmrc.vapingdutyfinance.models.payments.StartPaymentRequest
import uk.gov.hmrc.vapingdutyfinance.services.PaymentService

import javax.inject.{Inject, Singleton}
import scala.concurrent.{ExecutionContext, Future}

@Singleton
class PaymentController @Inject()(
  cc: ControllerComponents,
  authorisedAction: AuthorisedAction,
  paymentService: PaymentService
)(using ExecutionContext) extends BackendController(cc) with Logging {

  private val invalidRequestMessage = "Invalid request body"
  private val paymentErrorMessage   = "An error occurred while starting the payment"

  def startPayment(): Action[JsValue] = authorisedAction.async(parse.json) { implicit request =>
    request.body.validate[StartPaymentRequest].fold(
      errors => {
        logger.warn(s"Invalid StartPaymentRequest: $errors")
        Future.successful(BadRequest(Json.obj("error" -> invalidRequestMessage)))
      },
      paymentRequest =>
        paymentService.startPayment(paymentRequest)
          .map(response => Ok(Json.toJson(response)))
          .recover {
            case e: UpstreamErrorResponse =>
              logger.error(s"Error from pay-api: ${e.getMessage}", e)
              Status(e.statusCode)(Json.obj("error" -> paymentErrorMessage))
          }
    )
  }
}
