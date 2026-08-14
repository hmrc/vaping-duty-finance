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

package uk.gov.hmrc.vapingdutyfinance.services

import play.api.Logging
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.vapingdutyfinance.connectors.DirectDebitConnector
import uk.gov.hmrc.vapingdutyfinance.models.directdebit.{DirectDebitOrigin, StartDirectDebitRequest, StartDirectDebitResponse}

import javax.inject.{Inject, Singleton}
import scala.concurrent.{ExecutionContext, Future}

@Singleton
class DirectDebitService @Inject()(
  directDebitConnector: DirectDebitConnector
)(using ExecutionContext) extends Logging {

  def startDirectDebit(request: StartDirectDebitRequest, origin: DirectDebitOrigin)
                      (using HeaderCarrier): Future[StartDirectDebitResponse] =
    directDebitConnector.startDirectDebit(request, origin)
}