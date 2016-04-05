package dos.project4.model

import dos.project4.api
import dos.project4.common.{EncryptionInfo, NodeType}

object Photo {
  def apply(id:Long, photo:api.Photo) :Photo = {
    val newPhoto = new Photo(id, photo.edgeInfo.get.owner, photo.edgeInfo.get.ownerNodeType, photo.edgeInfo.get.createdBy)
    newPhoto.link = photo.link
    newPhoto.data = photo.data
    newPhoto.encryptionInfo = photo.encryptionInfo
    if(newPhoto.encryptionInfo.isDefined && !newPhoto.encryptionInfo.get.encKeys.contains(photo.edgeInfo.get.createdBy))
      println("alarm!!")
    newPhoto
  }
}

class Photo(override val id:Long,
            override val owner: Long,
            override val ownerNodeType:NodeType.NodeType,
            override val createdBy:Long)extends Edge(id){

  var link:Option[String] = None
  var data:Option[String] = None
  var encryptionInfo:Option[EncryptionInfo] = None

  def toApiObject():api.Photo = {
    api.Photo(Some(id), link , data, Some(api.Edge(owner,ownerNodeType, createdBy)), encryptionInfo)
  }
}

