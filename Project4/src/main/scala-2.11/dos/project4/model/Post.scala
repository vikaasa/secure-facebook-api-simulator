package dos.project4.model

import dos.project4.api
import dos.project4.common.{EncryptionInfo, NodeType}

object Post {
  def apply(id:Long, post:api.Post) :Post = {
    val newPost = new Post(id, post.edgeInfo.get.owner, post.edgeInfo.get.ownerNodeType, post.edgeInfo.get.createdBy)
    newPost.description = post.description
    newPost.link = post.link
    newPost.picture  = post.picture match {
      case Some(picture) => picture.id
      case None => None
    }
    newPost.message = post.message
    newPost.encryptionInfo = post.encryptionInfo
    newPost
  }
}

class Post(override val id:Long,
           override val owner: Long,
           override val ownerNodeType:NodeType.NodeType,
           override val createdBy:Long) extends Edge(id) {

  var message:String = null
  var picture:Option[Long] = None //Link to a picture having pictureId
  var description:Option[String] = None
  var link:Option[String] = None // Link associated with the post
//  def copy(post:api.Post):Unit = {
  //    description = post.description
  //    message = post.message
  //    link = post.link
  //    picture = post.picture
  //  }
  var encryptionInfo:Option[EncryptionInfo] = None



  def toApiObject():api.Post = {
    val picture = this.picture match {
      case Some(photoId) => Some(api.Photo(Some(photoId), None, None, None, None))
      case None => None
    }
    api.Post(Some(id),message,picture, description, link, Some(api.Edge(owner, ownerNodeType, createdBy)),encryptionInfo)
  }

}
