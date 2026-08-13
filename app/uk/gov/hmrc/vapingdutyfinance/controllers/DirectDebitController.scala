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

import play.api.Logging
import play.api.libs.json.{JsValue, Json}
import play.api.mvc.{Action, ControllerComponents}
import uk.gov.hmrc.play.bootstrap.backend.controller.BackendController
import uk.gov.hmrc.vapingdutyfinance.controllers.actions.AuthorisedAction
import uk.gov.hmrc.vapingdutyfinance.models.directdebit.{DirectDebitOrigin, StartDirectDebitRequest}
import uk.gov.hmrc.vapingdutyfinance.services.DirectDebitService

import javax.inject.{Inject, Singleton}
import scala.concurrent.ExecutionContext

@Singleton
class DirectDebitController @Inject()(
  cc: ControllerComponents,
  authorisedAction: AuthorisedAction,
  directDebitService: DirectDebitService
)(using ExecutionContext) extends BackendController(cc) with Logging {

  private val directDebitErrorMessage = "An error occurred while starting the direct debit journey"

  def startVpdConfirmation(): Action[JsValue] = authorisedAction.async(parse.json) { implicit request =>
    request.body.validate[StartDirectDebitRequest].fold(
      errors => {
        logger.warn(s"Invalid StartDirectDebitRequest JSON: $errors")
        BadRequest(Json.obj("error" -> "Invalid request body"))
      },
      startRequest =>
        directDebitService.startDirectDebit(startRequest, DirectDebitOrigin.VpdConfirmation)
          .map(response => Redirect(response.nextUrl))
          .recover { case e =>
            logger.error(s"Failed to start direct debit journey for vpdId=${request.vpdId}: ${e.getMessage}", e)
            InternalServerError(Json.obj("error" -> directDebitErrorMessage))
          }
    )
  }

  def startBta(): Action[JsValue] = authorisedAction.async(parse.json) { implicit request =>
    request.body.validate[StartDirectDebitRequest].fold(
      errors => {
        logger.warn(s"Invalid StartDirectDebitRequest JSON: $errors")
        BadRequest(Json.obj("error" -> "Invalid request body"))
      },
      startRequest =>
        directDebitService.startDirectDebit(startRequest, DirectDebitOrigin.Bta)
          .map(response => Redirect(response.nextUrl))
          .recover { case e =>
            logger.error(s"Failed to start direct debit journey for vpdId=${request.vpdId}: ${e.getMessage}", e)
            InternalServerError(Json.obj("error" -> directDebitErrorMessage))
          }
    )
  }
}