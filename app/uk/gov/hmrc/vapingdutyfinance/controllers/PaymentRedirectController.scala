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
import play.api.mvc.{Action, AnyContent, ControllerComponents}
import uk.gov.hmrc.play.bootstrap.backend.controller.BackendController
import uk.gov.hmrc.vapingdutyfinance.config.AppConfig
import uk.gov.hmrc.vapingdutyfinance.connectors.PayApiConnector
import uk.gov.hmrc.vapingdutyfinance.controllers.actions.AuthorisedAction
import uk.gov.hmrc.vapingdutyfinance.models.payments.StartPaymentRequest
import uk.gov.hmrc.vapingdutyfinance.services.FinancialDataService

import javax.inject.{Inject, Singleton}
import scala.concurrent.{ExecutionContext, Future}
import scala.math.BigDecimal.RoundingMode

@Singleton
class PaymentRedirectController @Inject()(
  cc: ControllerComponents,
  authorisedAction: AuthorisedAction,
  financialDataService: FinancialDataService,
  payApiConnector: PayApiConnector,
  appConfig: AppConfig
)(using ExecutionContext) extends BackendController(cc) with Logging {

  def pay(): Action[AnyContent] = authorisedAction.async { implicit request =>
    financialDataService.getPayments(request.vpdId, dateFrom = None, dateTo = None).flatMap { payments =>
      payments.totalAccountBalance match {
        case Some(amount) =>
          val paymentRequest = StartPaymentRequest(
            vapingDutyReference   = request.vpdId,
            amountInPence         = (amount * 100).setScale(0, RoundingMode.HALF_UP).toLong,
            chargeReferenceNumber = None,
            returnUrl             = appConfig.payReturnUrl,
            backUrl               = appConfig.payBackUrl
          )
          payApiConnector.startPayment(paymentRequest, isBtaCalling = true)
            .map(response => Redirect(response.nextUrl))
            .recover { case e =>
              logger.warn(s"Failed to start pay-api journey for vpdId=${request.vpdId}: ${e.getMessage}", e)
              Redirect(appConfig.payErrorUrl)
            }
        case None =>
          logger.warn(s"No totalAccountBalance found for vpdId=${request.vpdId}")
          Future.successful(Redirect(appConfig.payErrorUrl))
      }
    }
  }
}
