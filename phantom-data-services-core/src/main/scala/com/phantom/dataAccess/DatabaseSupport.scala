package com.phantom.dataAccess

import scala.slick.driver.MySQLDriver
import scala.slick.session.{ Database, Session }
import com.phantom.ds.DSConfiguration
import scala.concurrent.ExecutionContext
import java.util.Properties

trait DatabaseSupport extends DSConfiguration {

  private implicit def executionContext : ExecutionContext = scala.concurrent.ExecutionContext.Implicits.global

  var dbProps = new Properties()
  dbProps.setProperty("autoReconnect", "true")

  val db = Database.forURL(
    DBConfiguration.url,
    DBConfiguration.user,
    DBConfiguration.pass,
    dbProps,
    DBConfiguration.driver
  )

  // again, creating a DAL requires a Profile, which in this case is the MySQLDriver
  val dataAccessLayer = new DataAccessLayer(MySQLDriver)
  val phantomUsers = new PhantomUserDAO(dataAccessLayer, db)
  val conversations = new ConversationDAO(dataAccessLayer, db)
  val conversationItems = new ConversationItemDAO(dataAccessLayer, db)
  val contacts = new ContactDAO(dataAccessLayer, db)
  val sessions = new SessionDAO(dataAccessLayer, db)

  //users.purgeDB
  //dataAccessLayer.drop(db.createSession())
  dataAccessLayer.create(db.createSession())

}
