package com.phantom.dataAccess

/**
 * Created with IntelliJ IDEA.
 * User: aparrish
 * Date: 1/7/14
 * Time: 9:35 PM
 * To change this template use File | Settings | File Templates.
 */

import scala.slick.session.Database
import com.phantom.model.{ ConversationItem, Conversation }
import scala.concurrent.{ Future, ExecutionContext, future }

class ConversationDAO(dal : DataAccessLayer, db : Database)(implicit ec : ExecutionContext) extends BaseDAO(dal, db) {
  import dal._
  import dal.profile.simple._

  def createDB = dal.create
  def dropDB = dal.drop
  def purgeDB = dal.purge

  def insert(conversationItem : Conversation) : Conversation = {
    val id = ConversationTable.forInsert.insert(conversationItem)
    Conversation(Some(id), conversationItem.toUser, conversationItem.fromUser)
  }

  def insertAll(conversations : Seq[Conversation]) : Future[Seq[Conversation]] = {
    future {
      val b = ConversationTable.forInsert.insertAll(conversations : _*)
      b.zip(conversations).map {
        case (id, conversation) =>
          conversation.copy(id = Some(id))
      }
    }
  }

  def findByFromUserId(fromUserId : Long) : List[Conversation] = {
    val items = Query(ConversationTable) filter { _.fromUser === fromUserId }
    items.list()
  }
  def deleteById(conversationId : Long) : Int = {
    val deleteQuery = Query(ConversationTable) filter { _.id === conversationId }
    deleteQuery delete
  }
  def findById(conversationId : Long) : Conversation = {
    val items = Query(ConversationTable) filter { _.id === conversationId }
    items.first
  }
  def updateById(conversation : Conversation) : Int = {
    val updateQuery = Query(ConversationTable) filter { _.id === conversation.id }
    updateQuery.update(conversation)
  }
  def findConversationsAndItems(fromUserId : Long) : List[(Conversation, List[ConversationItem])] = {
    val conversationPairs = (for {
      c <- ConversationTable
      ci <- ConversationItemTable if c.id === ci.conversationId && c.fromUser === fromUserId
    } yield (c, ci)).list

    conversationPairs.groupBy(_._1).map {
      case (convo, cItem) => (convo, cItem.map(_._2))
    }.toList
  }

}
