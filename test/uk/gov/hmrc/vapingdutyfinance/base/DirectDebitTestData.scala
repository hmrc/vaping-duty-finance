/*
 * Copyright 2024 HM Revenue & Customs
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

package uk.gov.hmrc.vapingdutyfinance.base

import uk.gov.hmrc.vapingdutyfinance.models.directdebit.{DirectDebitOrigin, StartDirectDebitRequest, StartDirectDebitResponse}

trait DirectDebitTestData {

  val directDebitReturnUrl  = "http://localhost:16003/vaping-duty-account/direct-debit-return"
  val directDebitBackUrl    = "http://localhost:16003/vaping-duty-account/direct-debit-back"
  val directDebitNextUrl    = "http://pay-api.service/direct-debit/next-url"

  val directDebitOriginVpdConfirmation: DirectDebitOrigin = DirectDebitOrigin.VpdConfirmation
  val directDebitOriginBta: DirectDebitOrigin = DirectDebitOrigin.Bta

  val startDirectDebitRequest: StartDirectDebitRequest = StartDirectDebitRequest(
    returnUrl = directDebitReturnUrl,
    backUrl = directDebitBackUrl
  )

  val startDirectDebitResponse: StartDirectDebitResponse = StartDirectDebitResponse(
    nextUrl = directDebitNextUrl
  )
}