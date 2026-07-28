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

package uk.gov.hmrc.vapingdutyfinance.models

sealed trait MainTransactionType {
  def code: String
  def description: String
}

object MainTransactionType {
  case object PaymentOnAccount extends MainTransactionType {
    val code = "0060"
    val description = "Payment on Account"
  }
  
  def fromCode(code: String): Option[MainTransactionType] = code match {
    case "0060" => Some(PaymentOnAccount)
    case _ => None
  }
  
  val all: Seq[MainTransactionType] = Seq(PaymentOnAccount)
}