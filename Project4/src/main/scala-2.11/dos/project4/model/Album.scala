package dos.project4.model


import dos.project4.api
import dos.project4.common.NodeType

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object Album {
  def apply(id:Long, album:api.Album) :Album = {
    val newAlbum = new Album(id, album.edgeInfo.get.owner, album.edgeInfo.get.ownerNodeType, album.edgeInfo.get.createdBy)
     album.coverPhoto match {
      case Some(cover) => newAlbum.coverPhoto = Some(cover.id.get)
      case None =>
    }

    newAlbum.name = album.name
    album.photos.foreach((photo) => newAlbum.photos += photo.id.get )
    newAlbum
  }
}
class Album  (override val id:Long,
              override val owner: Long,
              override val ownerNodeType:NodeType.NodeType,
              override val createdBy:Long) extends Edge(id){

  var coverPhoto:Option[Long] = None
  var name:Option[String] = None
  var photos: mutable.Set[Long] = new mutable.HashSet[Long]


  def toApiObject():api.Album = {
    val photos = new ListBuffer[api.Photo]
    this.photos.foreach((photoId) => photos += api.Photo(Some(photoId), None, None, None, None))
    val coverPhoto = this.coverPhoto match {
      case Some(photoId) => Some(api.Photo(Some(photoId), None,None, None, None))
      case None => None
    }
    api.Album(Some(id),coverPhoto,name,Some(photos.size),photos.toList, Some(api.Edge(owner, ownerNodeType, createdBy)))
  }

}
