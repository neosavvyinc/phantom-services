package com.phantom.ds.photo

import spray.http.MediaTypes._
import com.phantom.model._
import com.phantom.ds.framework.httpx._
import spray.json._
import com.phantom.ds.DataHttpService
import com.phantom.ds.framework.auth.{ EntryPointAuthenticator, RequestAuthenticator }
import scala.concurrent.Future

trait PhotoEndpoint extends DataHttpService with PhantomJsonProtocol {
  this : RequestAuthenticator with EntryPointAuthenticator =>

  val photoService = PhotoService()

  def findPhoto = pathPrefix("photos") {
    authenticate(unverified _) { user =>
      get {
        respondWithMediaType(`application/json`) {
          complete(photoService.findAll.map(PhotoCategoryList(_)))
        }
      }
    }
  }

  val photoRoute = findPhoto

}
