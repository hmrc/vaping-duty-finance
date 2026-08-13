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
import uk.gov.hmrc.vapingdutyfinance.connectors.DirectDebitConnector
import uk.gov.hmrc.vapingdutyfinance.models.directdebit.DirectDebitOrigin

import scala.concurrent.Future

class DirectDebitServiceSpec extends SpecBase {

  val mockDirectDebitConnector: DirectDebitConnector = mock[DirectDebitConnector]

  val service = new DirectDebitService(mockDirectDebitConnector)

  "DirectDebitService must" - {
    "delegate to the connector with VpdConfirmation origin" in {
      when(mockDirectDebitConnector.startDirectDebit(eqTo(testStartDirectDebitRequest), eqTo(DirectDebitOrigin.VpdConfirmation))(using any()))
        .thenReturn(Future.successful(testStartDirectDebitResponse))

      whenReady(service.startDirectDebit(testStartDirectDebitRequest, DirectDebitOrigin.VpdConfirmation)) { result =>
        result mustBe testStartDirectDebitResponse
      }
    }

    "delegate to the connector with Bta origin" in {
      when(mockDirectDebitConnector.startDirectDebit(eqTo(testStartDirectDebitRequest), eqTo(DirectDebitOrigin.Bta))(using any()))
        .thenReturn(Future.successful(testStartDirectDebitResponse))

      whenReady(service.startDirectDebit(testStartDirectDebitRequest, DirectDebitOrigin.Bta)) { result =>
        result mustBe testStartDirectDebitResponse
      }
    }

    "propagate connector failures" in {
      val expectedException = new RuntimeException("Connector failure")

      when(mockDirectDebitConnector.startDirectDebit(any(), any())(using any()))
        .thenReturn(Future.failed(expectedException))

      whenReady(service.startDirectDebit(testStartDirectDebitRequest, DirectDebitOrigin.VpdConfirmation).failed) { exception =>
        exception mustBe expectedException
      }
    }
  }
}