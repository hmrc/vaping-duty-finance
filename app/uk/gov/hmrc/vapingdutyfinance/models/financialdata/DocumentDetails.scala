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

package uk.gov.hmrc.vapingdutyfinance.models.financialdata

import play.api.libs.json.{Json, OFormat}

import java.time.LocalDate

final case class DocumentDetails(
  documentNumber: Option[String],
  documentType: Option[String],
  chargeReferenceNumber: Option[String],
  businessPartnerNumber: Option[String],
  contractAccountNumber: Option[String],
  contractAccountCategory: Option[String],
  contractObjectNumber: Option[String],
  contractObjectType: Option[String],
  postingDate: Option[LocalDate],
  issueDate: Option[LocalDate],
  documentTotalAmount: Option[BigDecimal],
  documentClearedAmount: Option[BigDecimal],
  documentOutstandingAmount: Option[BigDecimal],
  documentInterestTotals: Option[DocumentInterestTotals],
  documentPenaltyTotals: Option[Seq[DocumentPenaltyTotals]],
  lineItemDetails: Option[Seq[LineItemDetails]]
)

object DocumentDetails {
  given format: OFormat[DocumentDetails] = Json.format[DocumentDetails]
}
