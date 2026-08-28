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

package uk.gov.hmrc.vapingdutyfinance.connectors.helpers

import org.mockito.Mockito.when
import uk.gov.hmrc.vapingdutyfinance.base.SpecBase
import uk.gov.hmrc.vapingdutyfinance.config.AppConfig

import java.util.Base64

class HIPAuthSpec extends SpecBase {

  private val TEST_CLIENT_ID = "test-client-id"
  private val TEST_SECRET = "test-secret"
  private val BASIC_PREFIX = "Basic "

  val mockAppConfig: AppConfig = mock[AppConfig]
  val hipAuth = new HIPAuth(mockAppConfig)

  "authorizationForFinancialData must" - {
    "return a correctly formatted Basic auth string" in {
      when(mockAppConfig.financialDataClientId).thenReturn(TEST_CLIENT_ID)
      when(mockAppConfig.financialDataSecret).thenReturn(TEST_SECRET)

      val result = hipAuth.authorizationForFinancialData()

      result must startWith(BASIC_PREFIX)
    }

    "encode credentials in the correct format" in {
      when(mockAppConfig.financialDataClientId).thenReturn(TEST_CLIENT_ID)
      when(mockAppConfig.financialDataSecret).thenReturn(TEST_SECRET)

      val result = hipAuth.authorizationForFinancialData()
      val encodedPart = result.stripPrefix(BASIC_PREFIX)
      val decodedBytes = Base64.getDecoder.decode(encodedPart)
      val decodedString = new String(decodedBytes, "UTF-8")

      decodedString mustBe s"$TEST_CLIENT_ID:$TEST_SECRET"
    }

    "use credentials from AppConfig" in {
      when(mockAppConfig.financialDataClientId).thenReturn(TEST_CLIENT_ID)
      when(mockAppConfig.financialDataSecret).thenReturn(TEST_SECRET)

      val expectedCredentials = s"$TEST_CLIENT_ID:$TEST_SECRET"
      val expectedEncoded = Base64.getEncoder.encodeToString(expectedCredentials.getBytes("UTF-8"))
      val expectedResult = s"$BASIC_PREFIX$expectedEncoded"

      val result = hipAuth.authorizationForFinancialData()

      result mustBe expectedResult
    }
  }
}