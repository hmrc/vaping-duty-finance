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

import com.github.tomakehurst.wiremock.WireMockServer
import com.github.tomakehurst.wiremock.client.WireMock
import com.github.tomakehurst.wiremock.client.WireMock.*
import com.github.tomakehurst.wiremock.core.WireMockConfiguration.wireMockConfig
import com.github.tomakehurst.wiremock.http.Fault

trait WireMockHelper {
  val wireMockPort: Int = 11111
  val wireMockHost: String = "localhost"
  val wireMockServer: WireMockServer = new WireMockServer(wireMockConfig().port(wireMockPort))

  def startWireMock(): Unit = {
    wireMockServer.start()
    WireMock.configureFor(wireMockHost, wireMockPort)
  }

  def stopWireMock(): Unit = wireMockServer.stop()
  def resetWireMock(): Unit = wireMockServer.resetAll()

  def stubPost(url: String, status: Int, responseBody: String): Unit =
    wireMockServer.stubFor(
      post(urlEqualTo(url))
        .willReturn(
          aResponse()
            .withStatus(status)
            .withBody(responseBody)
            .withHeader("Content-Type", "application/json")
        )
    )

  def stubPostFault(url: String): Unit =
    wireMockServer.stubFor(
      post(urlEqualTo(url))
        .willReturn(aResponse().withFault(Fault.EMPTY_RESPONSE))
    )

  def verifyPost(url: String): Unit =
    wireMockServer.verify(postRequestedFor(urlEqualTo(url)))

  def verifyPostWithoutRetry(url: String): Unit =
    wireMockServer.verify(1, postRequestedFor(urlEqualTo(url)))

  def verifyPostWithRetry(url: String): Unit =
    wireMockServer.verify(2, postRequestedFor(urlEqualTo(url)))
}
