package com.phantom.ds.dataAccess

import org.specs2.mutable._
import com.phantom.dataAccess.DatabaseSupport
import com.phantom.model.{ PhantomUser, Contact, ContactComponent }
import org.specs2.specification.BeforeAfter

class ContactDAOSpec extends BaseDAOSpec {

  sequential

  "ContactDAO" should {
    "support inserting a single contact" in withSetupTeardown {
      insertTestUsers
      contacts.insert(Contact(None, 1, 2, "friend")) must be_==(Contact(Some(1), 1, 2, "friend")).await
      contacts.insert(Contact(None, 2, 1, "friend")) must be_==(Contact(Some(2), 2, 1, "friend")).await
    }

    "support inserting a list of contacts" in withSetupTeardown {

      insertTestUsers

      val cs : List[Contact] = List(
        Contact(None, 1, 2, "friend"),
        Contact(None, 1, 3, "friend")
      )

      val res = contacts.insertAll(cs)

      res must be_==(
        List(
          Contact(Some(1), 1, 2, "friend"),
          Contact(Some(2), 1, 3, "friend")
        )
      ).await

      contacts.findAll must be_==(
        List(
          Contact(Some(1), 1, 2, "friend"),
          Contact(Some(2), 1, 3, "friend")
        )
      ).await
    }

    "should support deleting a user's contacts" in withSetupTeardown {

      val session = db.createSession

      val cs : List[Contact] = List(
        Contact(None, 1, 2, "friend"),
        Contact(None, 1, 3, "friend")
      )

      insertTestUsers
      contacts.insertAll(cs)

      contacts.deleteAll(1)(session) must be_==(2).await
    }
  }
}
