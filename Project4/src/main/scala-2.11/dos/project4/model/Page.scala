package dos.project4.model

import dos.project4.api

import scala.collection.mutable

object Page {
  def apply(id:Long, page:api.Page) :Page = {
    val newPage = new Page(id)
    newPage.description = page.description
    newPage.title = page.title
    newPage.website = page.website
    newPage.writtenBy = page.writtenBy.get
    newPage
  }
}

class Page(override val id:Long) extends Profile(id){
  var description:Option[String] = None
  var title:Option[String] = None
  var website:Option[String] = None
  var writtenBy:Long = 0
  var members:mutable.Set[Long] = new mutable.HashSet[Long]()

  def copy(page:api.Page):Unit = {
    description = page.description
    title = page.title
    website = page.website
  }

  def toApiObject():api.Page = {
    api.Page(Some(id),title,description,website,Some(writtenBy))
  }

}
