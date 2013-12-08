package com.phantom.ds.user

import spray.http.MediaTypes._
import spray.http.StatusCodes
import com.phantom.model._
import com.phantom.model.UserJsonImplicits._
import spray.json._
import com.phantom.ds.DataHttpService

trait UserEndpoint extends DataHttpService {

  val userService = UserService()

  val userRoute =
    pathPrefix("users" / "register") {
      post {
        respondWithMediaType(`application/json`)
        entity(as[UserRegistration]) {
          reg =>
            complete {
              userService.registerUser(reg)
            }
        }
      }
    } ~
      pathPrefix("users" / "login") {
        post {
          respondWithMediaType(`application/json`)
          entity(as[UserLogin]) {
            reg =>
              complete(userService.loginUser(reg))
          }
        }
      } ~
      pathPrefix("users" / LongNumber) { id =>
        get {
          respondWithMediaType(`application/json`) {
            complete {
              StatusCodes.OK
            }
          }
        }
      } ~
      pathPrefix("users" / LongNumber / "contacts") { id =>
        get {
          respondWithMediaType(`application/json`) {
            complete {
              StatusCodes.OK
            }
          }
        } ~
          post {
            respondWithMediaType(`application/json`) {
              entity(as[List[String]]) { contacts /* list of phone numbers */ =>
                complete {
                  StatusCodes.OK
                }
              }
            }
          }
      }

}
