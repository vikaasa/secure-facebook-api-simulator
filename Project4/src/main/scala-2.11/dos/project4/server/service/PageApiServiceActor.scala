package dos.project4.server.service

import dos.project4._
import dos.project4.api.FBException
import dos.project4.common.ActorNames
import dos.project4.model.DataStore
import dos.project4.server.service.PageApiServiceActor.{CreatePage, GetPage}

object PageApiServiceActor{
  case class GetPage(pageId:Long)
  case class CreatePage(page:api.Page, loggedInUser: api.User)
}



class PageApiServiceActor extends BaseApiServiceActor{
  override def receive: Receive = {
    case CreatePage(page, logggedInUser) =>
      if(!initialised)
        getNewIdRange()

      page.writtenBy = logggedInUser.id
      count += 1
      val newPage = model.Page(count,page)
      DataStore.pages += (count -> newPage)
      validateIdRange()
      sender() ! newPage.toApiObject()

    case GetPage(pageId) =>
      DataStore.pages.get(pageId) match {
        case Some(x) =>
          sender() ! x.toApiObject()
        case None => sender() ! FBException("Invalid Page")
      }


  }

  override protected def getServiceManagerRef: String = ActorNames.PageApiServiceManagerRef
}
