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

import org.mockito.ArgumentMatchers.{any, eq as eqTo}
import org.mockito.Mockito.when
import play.api.test.Helpers.*
import uk.gov.hmrc.http.UpstreamErrorResponse
import uk.gov.hmrc.vapingdutyfinance.base.SpecBase
import uk.gov.hmrc.vapingdutyfinance.connectors.PayApiConnector
import uk.gov.hmrc.vapingdutyfinance.models.payments.StartPaymentRequest
import uk.gov.hmrc.vapingdutyfinance.services.FinancialDataService

import scala.concurrent.Future

class PaymentRedirectControllerSpec extends SpecBase {

  val mockFinancialDataService: FinancialDataService = mock[FinancialDataService]
  val mockPayApiConnector: PayApiConnector           = mock[PayApiConnector]

  val controller = new PaymentRedirectController(
    cc,
    fakeAuthorisedAction,
    mockFinancialDataService,
    mockPayApiConnector,
    appConfig
  )

  "PaymentRedirectController" - {
    "pay must" - {
      "redirect to nextUrl for a single charge reference" in {
        when(mockFinancialDataService.getOutstandingAmount(eqTo(testVpdId), eqTo(Some(testChargeReferenceNumber)))(using any()))
          .thenReturn(Future.successful(Some(BigDecimal(45.74))))
        when(mockPayApiConnector.startPayment(eqTo(StartPaymentRequest(
          vapingDutyReference   = testVpdId,
          amountInPence         = 4574L,
          chargeReferenceNumber = Some(testChargeReferenceNumber),
          returnUrl             = appConfig.payReturnUrl,
          backUrl               = appConfig.payBackUrl
        )))(using any()))
          .thenReturn(Future.successful(testStartPaymentResponse))

        val result = controller.pay(Some(testChargeReferenceNumber))(fakeRequest)

        status(result) mustBe SEE_OTHER
        redirectLocation(result) mustBe Some(testNextUrl)
      }

      "redirect to nextUrl using the aggregate balance when no charge reference is given" in {
        when(mockFinancialDataService.getOutstandingAmount(eqTo(testVpdId), eqTo(None))(using any()))
          .thenReturn(Future.successful(Some(BigDecimal(82.50))))
        when(mockPayApiConnector.startPayment(eqTo(StartPaymentRequest(
          vapingDutyReference   = testVpdId,
          amountInPence         = 8250L,
          chargeReferenceNumber = None,
          returnUrl             = appConfig.payReturnUrl,
          backUrl               = appConfig.payBackUrl
        )))(using any()))
          .thenReturn(Future.successful(testStartPaymentResponse))

        val result = controller.pay(None)(fakeRequest)

        status(result) mustBe SEE_OTHER
        redirectLocation(result) mustBe Some(testNextUrl)
      }

      "redirect to the error page when no outstanding amount is found" in {
        when(mockFinancialDataService.getOutstandingAmount(eqTo(testVpdId), eqTo(Some(testChargeReferenceNumber)))(using any()))
          .thenReturn(Future.successful(None))

        val result = controller.pay(Some(testChargeReferenceNumber))(fakeRequest)

        status(result) mustBe SEE_OTHER
        redirectLocation(result) mustBe Some(appConfig.payErrorUrl)
      }

      "redirect to the error page when pay-api fails" in {
        when(mockFinancialDataService.getOutstandingAmount(eqTo(testVpdId), eqTo(Some(testChargeReferenceNumber)))(using any()))
          .thenReturn(Future.successful(Some(BigDecimal(45.74))))
        when(mockPayApiConnector.startPayment(any())(using any()))
          .thenReturn(Future.failed(UpstreamErrorResponse("Unexpected response from pay-api", INTERNAL_SERVER_ERROR)))

        val result = controller.pay(Some(testChargeReferenceNumber))(fakeRequest)

        status(result) mustBe SEE_OTHER
        redirectLocation(result) mustBe Some(appConfig.payErrorUrl)
      }
    }
  }
}
