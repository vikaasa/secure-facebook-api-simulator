package dos.project4.server.service

import akka.pattern.ask
import dos.project4._
import dos.project4.api.FBException
import dos.project4.common.{ActorNames, NodeType}
import dos.project4.model.DataStore
import dos.project4.server.FriendUtils
import dos.project4.server.service.PhotoApiServiceActor.PostPhoto
import dos.project4.server.service.PostApiServiceActor.{CreatePost, GetPosts}

import scala.collection.mutable.ListBuffer
import scala.concurrent.Future

object PostApiServiceActor{
  case class CreatePost(post:api.Post, edgeInfo:api.Edge, loggedInUser: api.User)
  case class GetPosts(id:Long, nodeType:NodeType.NodeType, loggedInUser: api.User)
}

class PostApiServiceActor extends BaseApiServiceActor with EdgeValidationService{

   private def addNewPost(post: api.Post, edgeInfo:api.Edge, loggedInUser:api.User) : model.Post ={
     if(!initialised)
       getNewIdRange()
     count += 1
     val newPost = model.Post(count, post)
     DataStore.posts.put(count, newPost)
     edgeInfo.ownerNodeType match{
       case NodeType.User =>
//         DataStore.users.get(edgeInfo.owner) foreach  {_.posts.prepend(newPost.id)}
         //Add the post to posts of all users who have access to it
         newPost.encryptionInfo foreach((encInfo)=>{
           encInfo.encKeys.keySet foreach ((userId)=>{
             DataStore.users.get(userId) foreach  {_.posts.prepend(newPost.id)}
           })
         })

       case NodeType.Page =>
         DataStore.pages.get(edgeInfo.owner) foreach  {_.posts.prepend(newPost.id)}
     }
     validateIdRange()
     newPost
   }

   //Validates the following
   //Allows only friends of a user to post on his profile
   //Allows the anybody to post content on a page
   override protected def validateEdgeCreatePermission(edgeInfo:api.Edge, loggedInUser: api.User):Boolean = {
      if(edgeInfo.ownerNodeType == NodeType.User &&
        !FriendUtils.isFriendOrSelf(loggedInUser.id.get, edgeInfo.owner)){
        false
      } else if(edgeInfo.ownerNodeType == NodeType.Page) {
        if(DataStore.pages.get(edgeInfo.owner).isEmpty)
          false
        else true
      } else true

   }

   override def receive: Receive =  {
     case CreatePost(post:api.Post, edgeInfo:api.Edge, loggedInUser: api.User) =>
       if(!validateEdgeCreatePermission(edgeInfo,loggedInUser))
         sender() ! FBException("Un-Authorized")
       else {
         post.edgeInfo = Some(edgeInfo)
         if(post.picture.isDefined){
           //Using comprehension to wait till the actor invocation completes
           var ownerId = loggedInUser.id.get
           if(edgeInfo.ownerNodeType == NodeType.Page)
             ownerId = edgeInfo.owner
           val photoEdgeInfo = api.Edge(ownerId,edgeInfo.ownerNodeType, edgeInfo.createdBy)
           val senderRef = sender()
           for {
             photo <- (context.actorSelection(ActorNames.PhotoApiServiceRef) ? PostPhoto(post.picture.get, photoEdgeInfo , loggedInUser)).mapTo[api.Photo]
             result <- Future{
               post.picture = Some(photo)
               val createdPost = addNewPost(post, edgeInfo, loggedInUser).toApiObject()
               createdPost.picture = Some(photo)
               senderRef ! createdPost
             }
           } yield result

         } else sender() ! addNewPost(post, edgeInfo, loggedInUser).toApiObject()

       }

     case GetPosts(nodeId:Long, nodeType:NodeType.NodeType, loggedInUser: api.User) =>
       if(!validateEdgeGetPermission(nodeId,nodeType,loggedInUser))
       sender() ! FBException("Un-Authorized")
     else {
       var posts = new ListBuffer[api.Post]()
       nodeType match {
         case NodeType.User =>
           DataStore.users.get(nodeId) foreach {_.posts.slice(0,20) foreach ((postId)=> {
             val post = DataStore.posts.get(postId)
             if(post.isDefined)
               posts +=  post.get.toApiObject()
           })}
         case NodeType.Page =>
           DataStore.pages.get(nodeId) foreach {_.posts.slice(0,20) foreach ((postId)=> {
             val post = DataStore.posts.get(postId)
             if(post.isDefined)
               posts +=  post.get.toApiObject()
           })}
       }

       posts foreach ((post) => {
         //For each post if picture is present retrieve the picture
         if(post.picture.isDefined){
           val picture = DataStore.photos.get(post.picture.get.id.get)
           if(picture.isDefined){
             post.picture = Some(picture.get.toApiObject())
             if(picture.get.encryptionInfo.isDefined) {
               //If encyrptionInfo is defined remove the encryptedSharedKey for other users while returning the data
               var encKeys = picture.get.encryptionInfo.get.encKeys
               encKeys = encKeys.filterKeys( _ == loggedInUser.id.get)
               picture.get.encryptionInfo.get.encKeys = encKeys
             }
           }

         }
//         if(post.encryptionInfo.isDefined) {
//          //If encyrptionInfo is defined remove the encryptedSharedKey for other users while returning the data
//           var encKeys = post.encryptionInfo.get.encKeys
//           encKeys = encKeys.filterKeys( _ == loggedInUser.id.get)
//           post.encryptionInfo.get.encKeys = encKeys
//         }
         posts = posts.filter( (post) => {
           if(!post.encryptionInfo.isDefined)
             true
           else if(post.encryptionInfo.get.encKeys.isEmpty){

             false
           } else true
         })
       })
       sender() ! api.Posts(posts.toList)
     }


   }

  override protected def getServiceManagerRef: String = ActorNames.PostApiServiceManagerRef

}
