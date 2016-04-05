package dos.project4.server

import dos.project4.model.DataStore

object FriendUtils {
  // Checks if the user with friendId is a friend of user with userId
  def isFriendOrSelf(userId:Long, friendId:Long) : Boolean = {
    if(userId == friendId)
      true
    else{
      DataStore.users.get(userId) match{
        case Some(user) =>
          if(user.friends.contains(friendId))
            true
          else false
        case None => false
      }
    }
  }
}
