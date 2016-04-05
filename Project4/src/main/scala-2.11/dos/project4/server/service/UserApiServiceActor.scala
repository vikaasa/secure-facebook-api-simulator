package dos.project4.server.service

import dos.project4._
import dos.project4.api.{FBException, FriendList, Success}
import dos.project4.common.ActorNames
import dos.project4.model.DataStore
import dos.project4.server.FriendUtils
import dos.project4.server.service.UserApiServiceActor._

import scala.collection.mutable.ListBuffer
import scala.util.Random

object UserApiServiceActor{

  case class GetUser(id:Long, loggedInUser: api.User)
  case class UpdateUser(user:api.User, loggedInUser: api.User)
  case class CreateUser(user:api.User)
  case class AddFriend(friend_id:Long, userId:Long, loggedInUser: api.User)
  case class GetFriends(userId:Long, loggedInUser: api.User)
  case class BootStrapFriendList(avgFriends:Int)

}

class UserApiServiceActor extends BaseApiServiceActor{

  override def receive: Receive = {
    case GetUser(id, loggedInUser) =>
      if(FriendUtils.isFriendOrSelf(loggedInUser.id.get, id)){
        DataStore.users.get(id) match {
          case Some(x) =>
            sender() ! x.toApiObject()
          case None => FBException("Invalid User")
        }

      } else sender() ! FBException("Un-Authorized")
    case UpdateUser(user, loggedInUser) =>
      if(user.id.get != loggedInUser.id.get)
        sender() ! FBException("Un-Authorized")
      else {
        val modelUser = DataStore.users.get(user.id.get)
        modelUser.get.copy(user)
        sender() ! modelUser.get.toApiObject() //update always passes
      }
    case CreateUser(user:api.User) =>
      if(!initialised){
        getNewIdRange()
      }
      count += 1
      val newUser = model.User(count,user)
      DataStore.users += (count -> newUser)
      validateIdRange()
      sender() ! newUser.toApiObject()

    case AddFriend(friendId:Long, userId:Long, loggedInUser:api.User) =>
      if(loggedInUser.id.get != userId){
        sender() ! FBException("Un-Authorized")
      } else {
        DataStore.users.get(loggedInUser.id.get) match {
          case Some(currentUser) =>
            if(!currentUser.friends.contains(friendId) && DataStore.users.contains(friendId)){
              currentUser.friends.put(friendId, "") //TODO:Empty for now, add the encrypted key for this friend in the next phase
              DataStore.users.get(friendId).get.friends.put(loggedInUser.id.get, "") //TODO:Empty for now, add the encrypted key for this friend in the next phase
              sender() ! Success()
            } else {
              sender() ! FBException("Already a Friend / Invalid user-id")
            }

          case _ =>
            sender() ! FBException("Failed")
        }
      }
    case BootStrapFriendList(avgFriends) =>
      val startTime = System.currentTimeMillis()
      val userIds = DataStore.users.keySet.toList
      val numUsers = userIds.size
      if(numUsers > 10){
        var connectionCount = 0L
        while(connectionCount/numUsers < avgFriends){
          val user = DataStore.users.get(userIds(Random.nextInt(numUsers))).get
          var randomFriendId = 0L
          do {
            randomFriendId = userIds(Random.nextInt(numUsers))
          }while(user.friends.contains(randomFriendId))
          user.friends.put(randomFriendId, "")
          DataStore.users.get(randomFriendId).get.friends.put(user.id, "")
          connectionCount += 2
        }
//        println("Time taken: "+ (System.currentTimeMillis() - startTime)/1000.0)
        sender()! api.Success()
      } else sender () ! FBException("InvalidInput")



    case GetFriends(userId:Long, loggedInUser:api.User) =>
      if(!FriendUtils.isFriendOrSelf(loggedInUser.id.get, userId)){
        sender() ! FBException("Un-Authorized")
      } else {
        DataStore.users.get(userId) match {
          case Some(user) =>
            val friends = new ListBuffer[api.User]()
            user.friends.keys foreach((friendId)=> {
              DataStore.users.get(friendId) match {
                case Some(friend) => friends += friend.toApiObject()
                case _ =>
              }
            })
            sender() ! FriendList(friends.toList)
          case _ =>
            sender() ! FBException("Failed")
        }
      }


  }

  override protected def getServiceManagerRef: String = ActorNames.UserApiServiceManagerRef
}
