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

import org.mockito.ArgumentMatchers.{any, eq as eqTo}
import org.mockito.Mockito.when
import uk.gov.hmrc.vapingdutyfinance.base.SpecBase
import uk.gov.hmrc.vapingdutyfinance.connectors.PayApiConnector
import uk.gov.hmrc.vapingdutyfinance.models.payments.PaymentOrigin

import scala.concurrent.Future

class PaymentServiceSpec extends SpecBase {

  val mockConnector: PayApiConnector = mock[PayApiConnector]

  val service = PaymentService(mockConnector)

  "PaymentService" - {

    "startPayment must" - {
      "delegate to the connector with PaymentOrigin.Vpd" in {
        when(mockConnector.startPayment(eqTo(testStartPaymentRequest), eqTo(PaymentOrigin.Vpd))(using any()))
          .thenReturn(Future.successful(testStartPaymentResponse))

        whenReady(service.startPayment(testStartPaymentRequest)) { result =>
          result mustBe testStartPaymentResponse
        }
      }
    }

    "startBtaPayment must" - {
      "call pay-api with PaymentOrigin.Bta and return the response for a positive balance" in {
        when(mockConnector.startPayment(eqTo(testStartPaymentRequest), eqTo(PaymentOrigin.Bta))(using any()))
          .thenReturn(Future.successful(testStartPaymentResponse))

        whenReady(service.startBtaPayment(testStartPaymentRequest)) { result =>
          result mustBe testStartPaymentResponse
        }
      }
    }
  }
}
