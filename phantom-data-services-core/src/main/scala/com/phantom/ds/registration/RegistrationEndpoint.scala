package com.phantom.ds.registration

import com.phantom.ds.{ BasicCrypto, DataHttpService }
import com.phantom.ds.framework.httpx.PhantomJsonProtocol
import com.phantom.ds.framework.auth.EntryPointAuthenticator
import spray.http.MediaTypes._
import com.phantom.model.{ UserRegistrationRequest, RegistrationVerification, UserRegistration }

trait RegistrationEndpoint extends DataHttpService
    with PhantomJsonProtocol with BasicCrypto { this : EntryPointAuthenticator =>

  val registrationService = RegistrationService()

  def registrationEndpoint = pathPrefix("users" / "register") {
    authenticate(enter _) {
      bool =>
        post {
          respondWithMediaType(`application/json`)
          entity(as[UserRegistrationRequest]) {
            reg =>
              log.trace(s"registering $reg")
              complete(registrationService.register(UserRegistration(
                decryptField(reg.email).toLowerCase,
                decryptLocalDate(reg.birthday),
                decryptField(reg.password)
              )))
          }
        }
    }
  }

  def twilioVerificationEndpoint = pathPrefix("users" / "verification") { //lack of auth..this is twilio based...TODO: investigate security options here
    post {
      formFields(
        'AccountSid.as[String],
        'MessageSid.as[String],
        'From.as[String],
        'To.as[String],
        'Body.as[String],
        'NumMedia.as[Int]) {
          (messageSid, accountSid, from, to, body, numMedia) =>
            complete {
              registrationService.verifyRegistration(
                RegistrationVerification(messageSid, accountSid, from, to, body, numMedia))
            }
        }
    }

  }

  def nexmoVerificationEndpoint = pathPrefix("users" / "verification") { // this is for the nexmo verification method
    get {
      parameters(
        'messageId.as[String] ? "",
        'msisdn.as[String] ? "",
        'to.as[String] ? "",
        'text.as[String] ? "") {
          (messageId, msisdn, to, text) =>
            complete {
              val s = new scala.collection.immutable.StringOps(to)
              val toSane = {
                if (s.startsWith("+"))
                  to
                else
                  "+" + to
              }

              val s1 = new scala.collection.immutable.StringOps(msisdn)
              val fromSane = {
                if (s1.startsWith("+"))
                  msisdn
                else
                  "+" + msisdn
              }

              registrationService.verifyRegistration(
                RegistrationVerification(messageId, "", fromSane, toSane, text, 0))
            }
        }
    }

  }

  val registrationRoute =
    registrationEndpoint ~
      twilioVerificationEndpoint ~
      nexmoVerificationEndpoint

}