package com.phantom.ds.conversation

import spray.http.MediaTypes._
import com.phantom.ds.{ BasicCrypto, DataHttpService }
import com.phantom.model.{ ConversationStartRequest, ConversationRespondRequest }
import com.phantom.ds.framework.auth.RequestAuthenticator
import akka.actor.ActorRef
import com.phantom.model.Paging
import com.phantom.ds.integration.amazon.S3Service

/**
 * Created by Neosavvy
 *
 * User: adamparrish
 * Date: 12/7/13
 * Time: 2:37 PM
 */
trait ConversationEndpoint extends DataHttpService with BasicCrypto {
  this : RequestAuthenticator =>

  def twilioActor : ActorRef

  def appleActor : ActorRef

  def s3Service : S3Service

  val conversationService = ConversationService(twilioActor, appleActor, s3Service) //need a better way of injecting services..trait!
  val conversation = "conversation"

  def findFeed = pathPrefix(conversation) {
    authenticate(verified _) { user =>
      get {
        parameters('page.as[Int] ? -1, 'size.as[Int] ? -1) { (page, size) =>
          respondWithMediaType(`application/json`) {
            complete(
              //TODO: FeedFolder should also sanitize(maybe)
              conversationService.findFeed(user.id.get, Paging(page, size)) flatMap { resolvedFeed =>
                conversationService.sanitizeFeed(resolvedFeed, user)
              }
            )
          }
        }
      }
    }
  }

  def startConversation = {
    val ByteJsonFormat = null
    pathPrefix(conversation) {
      path("start") {
        authenticate(verified _) { user =>
          post {
            formFields('image.as[Array[Byte]], 'imageText, 'toUsers.as[String]) { (image, imageText, toUsers) =>
              complete {
                val encryptedToUsers : Seq[String] = toUsers.split(",")
                val decryptedToUsers : Seq[String] = encryptedToUsers.map { fieldVal : String =>
                  decryptField(fieldVal)
                }

                conversationService.saveFileForConversationId(image, user.id.get).map { url =>
                  conversationService.startConversation(
                    user.id.get,
                    decryptedToUsers.toSet,
                    imageText,
                    url
                  )
                }
              }
            }
          }
        }
      }
    }
  }

  def startStockConversation = {
    pathPrefix(conversation) {
      path("start" / "stock") {
        authenticate(verified _) { user =>
          post {
            respondWithMediaType(`application/json`) {
              entity(as[ConversationStartRequest]) { req =>

                complete {
                  val decryptedToUsers : Seq[String] = req.toUsers.map(decryptField(_))
                  conversationService.findPhotoUrlById(req.imageId).map { photoUrl : Option[String] =>
                    photoUrl.map { url : String =>
                      conversationService.startConversation(
                        user.id.get,
                        decryptedToUsers.toSet,
                        req.imageText,
                        url
                      )
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }

  def respondToConversation = {
    val ByteJsonFormat = null

    import spray.httpx.encoding.{ NoEncoding, Gzip }
    pathPrefix(conversation) {
      path("respond") {
        authenticate(verified _) { user =>
          post {
            formFields('image.as[Array[Byte]], 'imageText, 'convId.as[Long]) { (image, imageText, convId) =>
              complete {
                conversationService.saveFileForConversationId(image, convId).map { url =>
                  conversationService.respondToConversation(
                    user.id.get,
                    convId,
                    imageText,
                    url
                  )
                }
              }
            }
          }
        }
      }
    }
  }

  def respondToStockConversation = {
    pathPrefix(conversation) {
      path("respond" / "stock") {
        authenticate(verified _) { user =>
          post {
            respondWithMediaType(`application/json`) {

              // TO DO change this to ConversationContinueRequest (or something)
              entity(as[ConversationRespondRequest]) { req =>
                complete {
                  conversationService.findPhotoUrlById(req.imageId).map { photoUrl : Option[String] =>
                    photoUrl.map { url : String =>
                      conversationService.respondToConversation(
                        user.id.get,
                        req.convId,
                        req.imageText,
                        url)
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }

  def blockByConversationId = {
    pathPrefix(conversation) {
      path("block" / IntNumber) { id =>
        authenticate(verified _) { user =>
          post {
            respondWithMediaType(`application/json`) {
              complete {
                conversationService.blockByConversationId(user.id.get, id)
              }
            }
          }
        }
      }
    }
  }

  def setConversationAsViewed = {
    pathPrefix(conversation) {
      path("view" / IntNumber) { id =>
        authenticate(verified _) { user =>
          post {
            respondWithMediaType(`application/json`) {
              complete {

                val userId = user.id.get
                log.debug(s"marking the $id conversation item as viewed for user $userId")

                conversationService.viewConversationItem(id, userId)
              }
            }
          }
        }
      }
    }
  }

  def deleteConversation = {
    pathPrefix(conversation) {
      path("delete" / IntNumber) { id =>
        authenticate(verified _) { user =>
          delete {
            respondWithMediaType(`application/json`) {
              complete {
                conversationService.deleteConversation(user.id.get, id)
              }
            }
          }
        }

      }
    }
  }

  def deleteConversationItem = {
    pathPrefix(conversation) {
      path("deleteitem" / IntNumber) { id =>
        authenticate(verified _) { user =>
          delete {
            respondWithMediaType(`application/json`) {
              complete {
                conversationService.deleteConversationItem(user.id.get, id)
              }
            }
          }
        }
      }
    }
  }

  val conversationRoute = findFeed ~
    startConversation ~
    startStockConversation ~
    respondToConversation ~
    respondToStockConversation ~
    blockByConversationId ~
    setConversationAsViewed ~
    deleteConversation ~
    deleteConversationItem

}
