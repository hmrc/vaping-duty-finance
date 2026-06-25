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

package uk.gov.hmrc.vapingdutyfinance.config

import javax.inject.{Inject, Singleton}
import play.api.Configuration
import uk.gov.hmrc.play.bootstrap.config.ServicesConfig

@Singleton
class AppConfig @Inject()(config: Configuration, servicesConfig: ServicesConfig) {

  val appName: String = config.get[String]("appName")
  val enrolmentServiceName: String = config.get[String]("enrolment.serviceName")
  val enrolmentIdentifierKey: String = config.get[String]("enrolment.identifierKey")

  // Financial Data API Configuration
  private val financialDataServiceName = "financial-data"
  val financialDataBaseUrl: String = servicesConfig.baseUrl(financialDataServiceName)
  val financialDataEndpoint = "/RESTAdapter/cross-regime/taxpayer/financial-data/query"
  val useStaticFinancialData: Boolean = config.get[Boolean]("microservice.services.financial-data.use-static-data")
  
  def financialDataUrl: String = s"$financialDataBaseUrl$financialDataEndpoint"

  // VPD Constants
  val taxRegimeVpd = "VPD"
  val idTypeVpd = "ZVPD"
  val originatingSystemVpd = "MDTP-VPD"
  val transmittingSystem = "HIP"
  
  // Date Range Configuration
  val defaultDateRangeMonths: Int = 12
  
  // Selection Criteria Defaults
  val dateTypePosting = "POSTING"
  val includeClearedItemsDefault = true
  val includeStatisticalItemsDefault = false
  val includePaymentOnAccountDefault = false
  
  // Data Enrichment Defaults
  val addRegimeTotalisationDefault = true
  val addLockInformationDefault = false
  val addPenaltyDetailsDefault = true
  val addPostedInterestDetailsDefault = false
  val addAccruingInterestDetailsDefault = true
  
}
