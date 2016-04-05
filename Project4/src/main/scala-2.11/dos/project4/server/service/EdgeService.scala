package dos.project4.server.service

import dos.project4.api
import dos.project4.common.NodeType
import dos.project4.model.DataStore
import dos.project4.server.FriendUtils

//A trait that defines default validations for edgeServices like photos, albums and Posts
trait EdgeValidationService{

  //Allow only the loggedInUser to post items on his profile
  //Allow anybody to post items on a page
  protected def validateEdgeCreatePermission(edgeInfo:api.Edge, loggedInUser: api.User):Boolean = {
    if(edgeInfo.ownerNodeType == NodeType.User &&
      loggedInUser.id.get != edgeInfo.owner){
      false
    } else if(edgeInfo.ownerNodeType == NodeType.Page) {
      if(DataStore.pages.get(edgeInfo.owner).isDefined) // Page should exist
        true
      else false
    } else true

  }

  //Allow only the friends of loggedInUser to Get items
  //Allow public page access
  protected def validateEdgeGetPermission(ownerId:Long, nodeType:NodeType.NodeType, loggedInUser: api.User):Boolean = {
    if(nodeType == NodeType.User &&
      !FriendUtils.isFriendOrSelf(loggedInUser.id.get, ownerId)){
      false
    }
    else true
  }

}
