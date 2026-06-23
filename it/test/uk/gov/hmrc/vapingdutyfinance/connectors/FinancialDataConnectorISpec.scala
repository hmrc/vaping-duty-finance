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

package uk.gov.hmrc.vapingdutyfinance.connectors

import play.api.http.Status.*
import play.api.libs.json.Json
import uk.gov.hmrc.http.{BadRequestException, NotFoundException, UpstreamErrorResponse}
import uk.gov.hmrc.vapingdutyfinance.base.{ConnectorTestHelpers, SpecBase}
import uk.gov.hmrc.vapingdutyfinance.config.AppConfig
import uk.gov.hmrc.vapingdutyfinance.models.financialdata.*
import uk.gov.hmrc.vapingdutyfinance.utils.FakeUUIDGenerator

import java.time.{Instant, LocalDate}

class FinancialDataConnectorISpec extends SpecBase with ConnectorTestHelpers {

  override protected val endpointName = "financial-data"

  val testRequest: FinancialDataRequest = FinancialDataRequest(
    taxRegime = "VPD",
    taxpayerInformation = TaxpayerInformation(idType = "ZVPD", idNumber = testVpdId),
    selectionCriteria = SelectionCriteria(
      dateRange = DateRange(dateType = "POSTING", dateFrom = LocalDate.of(2024, 1, 1), dateTo = LocalDate.of(2024, 12, 31)),
      includeClearedItems = true,
      includeStatisticalItems = false,
      includePaymentOnAccount = false
    ),
    dataEnrichment = DataEnrichmentOptions(
      addRegimeTotalisation = true,
      addLockInformation = false,
      addPenaltyDetails = true,
      addPostedInterestDetails = false,
      addAccruingInterestDetails = true
    )
  )

  val testSuccessResponse: FinancialDataResponse = FinancialDataResponse(
    success = FinancialDataSuccess(
      processingDate = Instant.parse("2026-10-01T10:15:10Z"),
      financialData = Some(FinancialData(
        totalisation = Some(Totalisation(
          regimeTotalisation = Some(RegimeTotalisation(
            totalAccountOverdue = BigDecimal("1000.0"),
            totalAccountNotYetDue = BigDecimal("250.0"),
            totalAccountCredit = BigDecimal("0.0"),
            totalAccountBalance = BigDecimal("1250.0"))
          ))
        ),
        documentDetails = Some(Seq(DocumentDetails(
          documentNumber = Some("187346702498"),
          documentType = Some("TRM New Charge"),
          chargeReferenceNumber = Some("XP001286394838"),
          businessPartnerNumber = Some("100893731"),
          contractAccountNumber = Some("900726630"),
          contractAccountCategory = Some("Excise"),
          contractObjectNumber = Some("104920928302302"),
          contractObjectType = Some("ZVPD"),
          postingDate = Some(LocalDate.of(2026, 10, 1)),
          issueDate = Some(LocalDate.of(2026, 10, 1)),
          documentTotalAmount = Some(BigDecimal("100.0")),
          documentClearedAmount = Some(BigDecimal("100.0")),
          documentOutstandingAmount = Some(BigDecimal("0.0")),
          documentInterestTotals = None,
          documentPenaltyTotals = None,
          lineItemDetails = None))))
      )))

  val testErrorResponse: FinancialDataErrorResponse = FinancialDataErrorResponse(
    errors = FinancialDataError(
      processingDate = Instant.parse("2026-10-01T10:15:10Z"),
      code = "018",
      text = "No data found"
    )
  )

  "FinancialDataConnector" - {
    "getFinancialData must" - {
      "return a FinancialDataResponse on 201 Created with valid response" in new SetUp {
        stubPost(path, CREATED, Json.toJson(testSuccessResponse).toString())
        whenReady(connector.getFinancialData(testRequest)) { result =>
          result mustBe Right(testSuccessResponse)
          verifyPost(path)
        }
      }

      "fail with UpstreamErrorResponse on 422 Unprocessable Entity" in new SetUp {
        stubPost(path, UNPROCESSABLE_ENTITY, "")

        whenReady(connector.getFinancialData(testRequest).failed) { exception =>
          exception mustBe an[UpstreamErrorResponse]
          exception.asInstanceOf[UpstreamErrorResponse].statusCode mustBe UNPROCESSABLE_ENTITY
          verifyPost(path)
        }
      }

      "fail with UpstreamErrorResponse on 201 with invalid JSON" in new SetUp {
        stubPost(path, CREATED, """{"invalid": "json"}""")

        whenReady(connector.getFinancialData(testRequest).failed) { exception =>
          exception mustBe an[UpstreamErrorResponse]
          exception.asInstanceOf[UpstreamErrorResponse].statusCode mustBe INTERNAL_SERVER_ERROR
          verifyPost(path)
        }
      }

      "fail with UpstreamErrorResponse on 200 OK instead of 201 Created" in new SetUp {
        stubPost(path, OK, Json.toJson(testSuccessResponse).toString())

        whenReady(connector.getFinancialData(testRequest).failed) { exception =>
          exception mustBe an[UpstreamErrorResponse]
          exception.asInstanceOf[UpstreamErrorResponse].statusCode mustBe OK
          exception.asInstanceOf[UpstreamErrorResponse].message must include("Unexpected response from financial data API")
          verifyPost(path)
        }
      }

      "fail with UpstreamErrorResponse on 400 Bad Request" in new SetUp {
        stubPost(path, BAD_REQUEST, """{"error": "bad request"}""")

        whenReady(connector.getFinancialData(testRequest).failed) { exception =>
          exception mustBe an[BadRequestException]
          exception.asInstanceOf[BadRequestException].responseCode mustBe BAD_REQUEST
          verifyPost(path)
        }
      }

      "fail with UpstreamErrorResponse on 401 Unauthorized" in new SetUp {
        stubPost(path, UNAUTHORIZED, "")

        whenReady(connector.getFinancialData(testRequest).failed) { exception =>
          exception mustBe an[UpstreamErrorResponse]
          exception.asInstanceOf[UpstreamErrorResponse].statusCode mustBe UNAUTHORIZED
          verifyPost(path)
        }
      }

      "fail with UpstreamErrorResponse on 403 Forbidden" in new SetUp {
        stubPost(path, FORBIDDEN, "")

        whenReady(connector.getFinancialData(testRequest).failed) { exception =>
          exception mustBe an[UpstreamErrorResponse]
          exception.asInstanceOf[UpstreamErrorResponse].statusCode mustBe FORBIDDEN
          verifyPost(path)
        }
      }

      "fail with UpstreamErrorResponse on 404 Not Found" in new SetUp {
        stubPost(path, NOT_FOUND, "")

        whenReady(connector.getFinancialData(testRequest).failed) { exception =>
          exception mustBe an[NotFoundException]
          exception.asInstanceOf[NotFoundException].responseCode mustBe NOT_FOUND
          verifyPost(path)
        }
      }

      "fail with UpstreamErrorResponse on 500 Internal Server Error" in new SetUp {
        stubPost(path, INTERNAL_SERVER_ERROR, "")

        whenReady(connector.getFinancialData(testRequest).failed) { exception =>
          exception mustBe an[UpstreamErrorResponse]
          exception.asInstanceOf[UpstreamErrorResponse].statusCode mustBe INTERNAL_SERVER_ERROR
          verifyPost(path)
        }
      }

      "fail with UpstreamErrorResponse on network fault" in new SetUp {
        stubPostFault(path)

        whenReady(connector.getFinancialData(testRequest).failed) { exception =>
          exception mustBe an[Exception]
          verifyPost(path)
        }
      }

      "not retry on 400 Bad Request" in new SetUp {
        stubPost(path, BAD_REQUEST, "")

        whenReady(connectorWithRetry.getFinancialData(testRequest).failed) { exception =>
          exception mustBe an[BadRequestException]
          verifyPostWithoutRetry(path)
        }
      }
    }
    class SetUp extends ConnectorFixture {
      val fakeUUIDGenerator = FakeUUIDGenerator()
      val connector = FinancialDataConnector(httpClientV2, clock, appWithHttpClient.injector.instanceOf[AppConfig], fakeUUIDGenerator)
      val connectorWithRetry = FinancialDataConnector(httpClientV2, clock, appWithHttpClientAndRetry.injector.instanceOf[AppConfig], fakeUUIDGenerator)
      lazy val url = appWithHttpClient.injector.instanceOf[AppConfig].financialDataUrl
      lazy val path = new java.net.URL(url).getPath
    }
  }
}
