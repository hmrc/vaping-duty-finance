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

    "delegate to the connector with Vpd origin" in {
      when(mockConnector.startPayment(eqTo(testStartPaymentRequest), eqTo(PaymentOrigin.Vpd))(using any()))
        .thenReturn(Future.successful(testStartPaymentResponse))

      whenReady(service.startPayment(testStartPaymentRequest, PaymentOrigin.Vpd)) { result =>
        result mustBe testStartPaymentResponse
      }
    }

    "delegate to the connector with Bta origin" in {
      when(mockConnector.startPayment(eqTo(testStartPaymentRequest), eqTo(PaymentOrigin.Bta))(using any()))
        .thenReturn(Future.successful(testStartPaymentResponse))

      whenReady(service.startPayment(testStartPaymentRequest, PaymentOrigin.Bta)) { result =>
        result mustBe testStartPaymentResponse
      }
    }

    "propagate connector failures" in {
      val expectedException = new RuntimeException("Connector failure")

      when(mockConnector.startPayment(any(), any())(using any()))
        .thenReturn(Future.failed(expectedException))

      whenReady(service.startPayment(testStartPaymentRequest, PaymentOrigin.Vpd).failed) { exception =>
        exception mustBe expectedException
      }
    }
  }
}