/*
 * Copyright 2023 HM Revenue & Customs
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

package uk.gov.hmrc.vapingdutyfinance.controllers.actions

import org.apache.pekko.stream.testkit.NoMaterializer
import org.mockito.ArgumentMatchers.{any, eq as eqTo}
import org.mockito.Mockito.when
import org.scalatest.freespec.AnyFreeSpec
import play.api.mvc.*
import play.api.mvc.Results.Ok
import play.api.test.Helpers.*
import uk.gov.hmrc.auth.core.*
import uk.gov.hmrc.auth.core.AffinityGroup.Organisation
import uk.gov.hmrc.auth.core.AuthProvider.GovernmentGateway
import uk.gov.hmrc.auth.core.CredentialStrength.strong
import uk.gov.hmrc.auth.core.retrieve.v2.Retrievals.{authorisedEnrolments, internalId}
import uk.gov.hmrc.auth.core.retrieve.~
import uk.gov.hmrc.vapingdutyfinance.base.SpecBase

import scala.concurrent.Future

class AuthorisedActionSpec extends SpecBase {
  
  val enrolment               = "HMRC-VPD-ORG"
  val vpdIdKey                = "ZVPD"
  val testInternalId: String  = "internalId"
  val state                   = "Activated"
  val enrolments              = Enrolments(Set(Enrolment(enrolment, Seq(EnrolmentIdentifier(vpdIdKey, testVpdId)), state)))
  val emptyEnrolments         = Enrolments(Set.empty)
  val enrolmentsWithoutAppaId = Enrolments(Set(Enrolment(enrolment, Seq.empty, state)))
  val testContent             = "Test"

  private val bodyParsers = stubPlayBodyParsers(NoMaterializer)
  private val defaultBodyParser  = new BodyParsers.Default(bodyParsers)
  private val mockAuthConnector: AuthConnector       = mock[AuthConnector]

  val authorisedAction =
    new BaseAuthorisedAction(mockAuthConnector, appConfig, defaultBodyParser)

  val testAction: Request[_] => Future[Result] = { _ =>
    Future(Ok(testContent))
  }

  "invokeBlock must" - {
    "execute the block and return OK if authorised" in {
      when(
        mockAuthConnector.authorise(
          eqTo(
            AuthProviders(GovernmentGateway)
              and Enrolment(enrolment)
              and CredentialStrength(strong)
              and Organisation
              and ConfidenceLevel.L50
          ),
          eqTo(
            internalId and authorisedEnrolments
          )
        )(any(), any())
      )
        .thenReturn(Future(new ~(Some(testInternalId), enrolments)))

      val result: Future[Result] = authorisedAction.invokeBlock(fakeRequest, testAction)

      status(result)          mustBe OK
      contentAsString(result) mustBe testContent
    }

    "execute the block and throw AuthorisationException if cannot get the enrolment" in {
      when(
        mockAuthConnector.authorise(
          eqTo(
            AuthProviders(GovernmentGateway)
              and Enrolment(enrolment)
              and CredentialStrength(strong)
              and Organisation
              and ConfidenceLevel.L50
          ),
          eqTo(
            internalId and authorisedEnrolments
          )
        )(any(), any())
      )
        .thenReturn(Future(new ~(Some(testInternalId), emptyEnrolments)))

      val result: Future[Result] = authorisedAction.invokeBlock(fakeRequest, testAction)

      status(result) mustBe UNAUTHORIZED
    }

    "execute the block and throw AuthorisationException if cannot get the VPPAID enrolment" in {
      when(
        mockAuthConnector.authorise(
          eqTo(
            AuthProviders(GovernmentGateway)
              and Enrolment(enrolment)
              and CredentialStrength(strong)
              and Organisation
              and ConfidenceLevel.L50
          ),
          eqTo(
            internalId and authorisedEnrolments
          )
        )(any(), any())
      )
        .thenReturn(Future(new ~(Some(testInternalId), enrolmentsWithoutAppaId)))

      val result: Future[Result] = authorisedAction.invokeBlock(fakeRequest, testAction)

      status(result) mustBe UNAUTHORIZED
    }

    "return 401 unauthorized if the authConnector returns None as Internal Id" in {
      when(
        mockAuthConnector.authorise(
          eqTo(
            AuthProviders(GovernmentGateway)
              and Enrolment(enrolment)
              and CredentialStrength(strong)
              and Organisation
              and ConfidenceLevel.L50
          ),
          eqTo(
            internalId and authorisedEnrolments
          )
        )(any(), any())
      )
        .thenReturn(Future(new ~(None, enrolments)))

      val result: Future[Result] = authorisedAction.invokeBlock(fakeRequest, testAction)

      status(result) mustBe UNAUTHORIZED
    }
  }

  "return 401 unauthorized if there is an authorisation exception" in {
    List(
      InsufficientConfidenceLevel(),
      InsufficientEnrolments(),
      UnsupportedAffinityGroup(),
      UnsupportedCredentialRole(),
      UnsupportedAuthProvider(),
      IncorrectCredentialStrength(),
      InternalError(),
      BearerTokenExpired(),
      MissingBearerToken(),
      InvalidBearerToken(),
      SessionRecordNotFound()
    ).foreach { exception =>
      when(mockAuthConnector.authorise[Unit](any(), any())(any(), any())).thenReturn(Future.failed(exception))

      val result: Future[Result] = authorisedAction.invokeBlock(fakeRequest, testAction)

      status(result) mustBe UNAUTHORIZED
    }
  }

  "return the exception if there is any other exception" in {
    val msg = "Test Exception"

    when(mockAuthConnector.authorise[Unit](any(), any())(any(), any()))
      .thenReturn(Future.failed(new RuntimeException(msg)))

    val result = intercept[RuntimeException] {
      await(authorisedAction.invokeBlock(fakeRequest, testAction))
    }

    result.getMessage mustBe msg
  }
}
