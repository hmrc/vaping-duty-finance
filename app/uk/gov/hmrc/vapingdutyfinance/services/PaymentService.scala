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
import uk.gov.hmrc.vapingdutyfinance.config.AppConfig
import uk.gov.hmrc.vapingdutyfinance.connectors.PayApiConnector
import uk.gov.hmrc.vapingdutyfinance.models.payments.{PaymentOrigin, StartPaymentRequest, StartPaymentResponse}

import javax.inject.{Inject, Singleton}
import scala.concurrent.{ExecutionContext, Future}
import scala.math.BigDecimal.RoundingMode

@Singleton
class PaymentService @Inject()(
  payApiConnector: PayApiConnector,
  appConfig: AppConfig
)(using ExecutionContext) extends Logging {

  def startPayment(request: StartPaymentRequest)(using HeaderCarrier): Future[StartPaymentResponse] =
    payApiConnector.startPayment(request, PaymentOrigin.Vpd)

  def startBtaPayment(vpdId: String, totalAccountBalance: Option[BigDecimal])
                      (using HeaderCarrier): Future[Option[StartPaymentResponse]] =
    totalAccountBalance.filter(_ > 0) match {
      case Some(amount) =>
        payApiConnector.startPayment(buildStartPaymentRequest(vpdId, amount), PaymentOrigin.Bta).map(Some(_))
      case None =>
        logger.warn(s"No positive totalAccountBalance found for vpdId=$vpdId")
        Future.successful(None)
    }

  private[services] def buildStartPaymentRequest(vpdId: String, amount: BigDecimal): StartPaymentRequest =
    StartPaymentRequest(
      vapingDutyReference   = vpdId,
      amountInPence         = toAmountInPence(amount),
      chargeReferenceNumber = None,
      returnUrl             = appConfig.payReturnUrl,
      backUrl               = appConfig.payBackUrl
    )

  private[services] def toAmountInPence(amount: BigDecimal): Long =
    (amount * 100).setScale(0, RoundingMode.HALF_UP).toLong
}
