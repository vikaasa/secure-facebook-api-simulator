package dos.project4.model

import dos.project4._
import spray.http.DateTime

import scala.collection.mutable

object User {
  def apply(id:Long, user:api.User) :User = {
    val newUser = new User(id)
    newUser.firstName = user.firstName
    newUser.lastName = user.lastName
    newUser.email = user.email
    newUser.birthday = user.birthday
    newUser.gender = user.gender
    newUser.pubKey = user.pubKey
    newUser
  }


}

class User(override val id:Long) extends Profile(id){
  var firstName:String = null
  var lastName:String = null
  var email:Option[String] = None
  var birthday:Option[DateTime] = None
  var gender:Option[String] = None
  var pubKey:String = null
  val friends:mutable.Map[Long, String] = new mutable.HashMap[Long, String]()
  val pendingRequests:mutable.Set[Long] = new mutable.HashSet[Long]()

  def copy(user:api.User):Unit = {
    firstName = user.firstName
    lastName = user.lastName
    email = user.email
    gender = user.gender
    birthday  = user.birthday
  }

  def toApiObject():api.User = {
    api.User(Some(id), firstName, lastName, email, birthday, gender, pubKey)
  }
}

