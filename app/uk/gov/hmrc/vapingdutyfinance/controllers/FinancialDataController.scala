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
import play.api.libs.json.Json
import play.api.mvc.{Action, AnyContent, ControllerComponents}
import uk.gov.hmrc.play.bootstrap.backend.controller.BackendController
import uk.gov.hmrc.vapingdutyfinance.controllers.actions.AuthorisedAction
import uk.gov.hmrc.vapingdutyfinance.services.FinancialDataService

import java.time.LocalDate
import java.time.format.DateTimeParseException
import javax.inject.{Inject, Singleton}
import scala.concurrent.ExecutionContext

@Singleton
class FinancialDataController @Inject()(
                                         authorised: AuthorisedAction,
                                         service: FinancialDataService,
                                         cc: ControllerComponents
                                       )(using ExecutionContext) extends BackendController(cc) with Logging {

  def getOutstandingPayments(
                              dateFrom: Option[String],
                              dateTo: Option[String]
                            ): Action[AnyContent] = authorised.async { implicit request =>
    val parsedDateFrom = dateFrom.flatMap(parseDate)
    val parsedDateTo = dateTo.flatMap(parseDate)

    service.getOutstandingPayments(request.vpdId, parsedDateFrom, parsedDateTo).map {
      case Right(payments) =>
        Ok(Json.toJson(payments))
      case Left(error) =>
        logger.error(s"Error retrieving outstanding payments for vpdId=${request.vpdId}: $error")
        InternalServerError(Json.obj("error" -> "An error occurred while retrieving financial data"))
    }.recover { case ex =>
      logger.error(s"Unexpected error in getOutstandingPayments: ${ex.getMessage}", ex)
      InternalServerError(Json.obj("error" -> "An error occurred while retrieving financial data"))
    }
  }

  private def parseDate(dateString: String): Option[LocalDate] = {
    try {
      Some(LocalDate.parse(dateString))
    } catch {
      case _: DateTimeParseException =>
        logger.warn(s"Invalid date format: $dateString")
        None
    }
  }
}
