package com.phantom.ds.dataAccess

import org.specs2.mutable.Specification
import com.phantom.dataAccess.DatabaseSupport
import org.specs2.specification.BeforeAfter
import com.phantom.model._
import org.joda.time.{ DateTimeZone, LocalDate }
import java.util.UUID
import com.phantom.model.Conversation
import com.phantom.model.ConversationItem
import com.phantom.model.PhantomUser
import com.phantom.ds.user.Passwords

/**
 * Created with IntelliJ IDEA.
 * User: aparrish
 * Date: 1/9/14
 * Time: 9:34 PM
 * To change this template use File | Settings | File Templates.
 */
trait BaseDAOSpec extends Specification with DatabaseSupport {

  implicit val session = db.createSession

  object withSetupTeardown extends BeforeAfter {
    def before {
      dataAccessLayer.drop
      dataAccessLayer.create

    }

    def after {
      //      conversations.dropDB
    }
  }

  def setupConversationItems(convId : Long) : List[ConversationItem] = {
    val item1 = new ConversationItem(
      None, convId, "image1Url", "image1Text"
    )
    val item2 = new ConversationItem(
      None, convId, "image1Url", "image1Text"
    )
    val item3 = new ConversationItem(
      None, convId, "image1Url", "image1Text"
    )
    List(item1, item2, item3)
  }

  def createVerifiedUser(email : String, password : String) = {
    phantomUsers.insert(PhantomUser(None, UUID.randomUUID, email, Passwords.getSaltedHash(password), LocalDate.now(DateTimeZone.UTC), true, "", Verified))
  }

  def createUnverifiedUser(email : String, password : String) = {
    phantomUsers.insert(PhantomUser(None, UUID.randomUUID, email, Passwords.getSaltedHash(password), LocalDate.now(DateTimeZone.UTC), true, "", Unverified))
  }

  def createConversation(fromId : Long, toId : Long) : Conversation = {
    conversationDao.insert(Conversation(None, toId, fromId))
  }

  def insertTestUsers {
    val user1 = new PhantomUser(None, UUID.randomUUID(), "aparrish@neosavvy.com", "password", new LocalDate(1981, 8, 10), true, "111111")
    val user2 = new PhantomUser(None, UUID.randomUUID(), "ccaplinger@neosavvy.com", "password", new LocalDate(1986, 10, 12), true, "222222")
    val user3 = new PhantomUser(None, UUID.randomUUID(), "tewen@neosavvy.com", "password", new LocalDate(1987, 8, 16), true, "333333")
    val user4 = new PhantomUser(None, UUID.randomUUID(), "dhamlettneosavvy.com", "password", new LocalDate(1985, 5, 17), true, "1234567")
    val user5 = new PhantomUser(None, UUID.randomUUID(), "nick.sauro@gmail.com", "password", new LocalDate(1987, 8, 16), true, "1234567")
    val user6 = new PhantomUser(None, UUID.randomUUID(), "pablo.alonso@gmail.com", "password", new LocalDate(1987, 8, 16), true, "1234567")
    phantomUsers.insert(user1)
    phantomUsers.insert(user2)
    phantomUsers.insert(user3)
    phantomUsers.insert(user4)
    phantomUsers.insert(user5)
    phantomUsers.insert(user6)
  }

  def insertTestConversations {

    val conv1 = new Conversation(None, 1, 2)
    val conv2 = new Conversation(None, 3, 4)
    val conv3 = new Conversation(None, 5, 6)
    conversationDao.insert(conv1)
    conversationDao.insert(conv2)
    conversationDao.insert(conv3)

  }

  def insertTestConverationsWithItems {
    insertTestUsersAndConversations

    val conv1item1 = new ConversationItem(None, 1, "imageUrl1", "imageText1")
    val conv1item2 = new ConversationItem(None, 1, "imageUrl2", "imageText2")
    val conv1item3 = new ConversationItem(None, 1, "imageUrl3", "imageText3")

    val conv2item1 = new ConversationItem(None, 2, "imageUrl1", "imageText1")
    val conv2item2 = new ConversationItem(None, 2, "imageUrl2", "imageText2")
    val conv2item3 = new ConversationItem(None, 2, "imageUrl3", "imageText3")

    conversationItemDao.insert(conv1item1)
    conversationItemDao.insert(conv1item2)
    conversationItemDao.insert(conv1item3)
    conversationItemDao.insert(conv2item1)
    conversationItemDao.insert(conv2item2)
    conversationItemDao.insert(conv2item3)
  }

  def insertTestUsersAndConversations {
    insertTestUsers
    insertTestConversations
  }

}