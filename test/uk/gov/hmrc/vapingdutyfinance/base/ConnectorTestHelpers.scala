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

package uk.gov.hmrc.vapingdutyfinance.base

import org.scalatest.{BeforeAndAfterAll, BeforeAndAfterEach, Suite}
import play.api.Application
import play.api.inject.bind
import play.api.inject.guice.GuiceApplicationBuilder
import uk.gov.hmrc.http.client.HttpClientV2

trait ConnectorTestHelpers extends BeforeAndAfterAll with BeforeAndAfterEach with WireMockHelper {
  this: Suite with TestData =>

  protected val endpointName: String

  def getWireMockAppConfig(endpoints: Seq[String]): Map[String, Any] =
    endpoints.foldLeft(Map.empty[String, Any]) { (config, endpoint) =>
      config ++ Map(
        s"microservice.services.$endpoint.protocol" -> "http",
        s"microservice.services.$endpoint.host" -> "localhost",
        s"microservice.services.$endpoint.port" -> wireMockPort
      )
    }

  def getWireMockAppConfigWithRetry(endpoints: Seq[String]): Map[String, Any] =
    getWireMockAppConfig(endpoints) ++ Map(
      "http-verbs.retries.intervals" -> List("1ms", "1ms")
    )

  override def beforeAll(): Unit = {
    super.beforeAll()
    startWireMock()
  }

  override def afterAll(): Unit = {
    stopWireMock()
    super.afterAll()
  }

  override def beforeEach(): Unit = {
    super.beforeEach()
    resetWireMock()
  }

  trait ConnectorFixture {
    val httpClientV2: HttpClientV2 = app.injector.instanceOf[HttpClientV2]

    val appWithHttpClient: Application =
      GuiceApplicationBuilder()
        .configure(getWireMockAppConfig(Seq(endpointName)))
        .overrides(bind[HttpClientV2].toInstance(httpClientV2))
        .build()

    val appWithHttpClientAndRetry: Application =
      GuiceApplicationBuilder()
        .configure(getWireMockAppConfigWithRetry(Seq(endpointName)))
        .overrides(bind[HttpClientV2].toInstance(httpClientV2))
        .build()
  }
}
