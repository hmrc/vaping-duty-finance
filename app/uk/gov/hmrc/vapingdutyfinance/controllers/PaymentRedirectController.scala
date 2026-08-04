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
import play.api.libs.json.Json
import play.api.mvc.{Action, AnyContent, ControllerComponents}
import uk.gov.hmrc.play.bootstrap.backend.controller.BackendController
import uk.gov.hmrc.vapingdutyfinance.controllers.actions.AuthorisedAction
import uk.gov.hmrc.vapingdutyfinance.services.{FinancialDataService, PaymentService}

import javax.inject.{Inject, Singleton}
import scala.concurrent.{ExecutionContext, Future}

@Singleton
class PaymentRedirectController @Inject()(
  cc: ControllerComponents,
  authorisedAction: AuthorisedAction,
  financialDataService: FinancialDataService,
  paymentService: PaymentService
)(using ExecutionContext) extends BackendController(cc) with Logging {

  private val noPaymentDueMessage = "No outstanding balance to pay"
  private val paymentErrorMessage = "An error occurred while starting the payment"

  def pay(): Action[AnyContent] = authorisedAction.async { implicit request =>
    financialDataService.getPayments(request.vpdId, dateFrom = None, dateTo = None).flatMap { payments =>
      payments.totalAccountBalance.filter(_ > 0) match {
        case Some(amount) =>
          paymentService.startBtaPayment(request.vpdId, amount)
            .map(response => Redirect(response.nextUrl))
            .recover { case e =>
              logger.error(s"Failed to start pay-api journey for vpdId=${request.vpdId}: ${e.getMessage}", e)
              InternalServerError(Json.obj("error" -> paymentErrorMessage))
            }
        case None =>
          logger.warn(s"No positive totalAccountBalance found for vpdId=${request.vpdId}")
          Future.successful(BadRequest(Json.obj("error" -> noPaymentDueMessage)))
      }
    }
  }
}
