package dos.project4.server.service

import akka.pattern.ask
import dos.project4.api.FBException
import dos.project4.common.{ActorNames, NodeType}
import dos.project4.model.DataStore
import dos.project4.server.service.AlbumApiServiceActor.{CreateAlbum, GetAlbums}
import dos.project4.server.service.PhotoApiServiceActor.PostPhotos
import dos.project4.{api, model}

import scala.collection.mutable.ListBuffer
import scala.concurrent.Future

object AlbumApiServiceActor{
  case class CreateAlbum(album:api.Album, edgeInfo:api.Edge,loggedInUser: api.User)
  case class GetAlbums(id:Long, nodeType:NodeType.NodeType, loggedInUser: api.User)
}

class AlbumApiServiceActor extends BaseApiServiceActor with EdgeValidationService{

  private def addNewAlbum(album: api.Album, edgeInfo:api.Edge, loggedInUser:api.User) : model.Album ={
    if(!initialised)
      getNewIdRange()
    count += 1
    val newAlbum = model.Album(count, album)
    DataStore.albums.put(count, newAlbum)
    edgeInfo.ownerNodeType match{
      case NodeType.User =>
        DataStore.users.get(edgeInfo.owner) foreach  {_.albums.prepend(newAlbum.id)}
      case NodeType.Page =>
        DataStore.pages.get(edgeInfo.owner) foreach  {_.albums.prepend(newAlbum.id)}
    }
    validateIdRange()
    newAlbum
  }

  override def receive: Receive =  {
    case CreateAlbum(album, edgeInfo,loggedInUser) =>
      if(!validateEdgeCreatePermission(edgeInfo,loggedInUser))
        sender() ! FBException("Un-Authorized")
      else if(album.photos.size <1)
        sender() ! FBException("Invalid Album")
      else {
        album.edgeInfo = Some(edgeInfo)
        if(album.coverPhoto.isDefined || album.photos.nonEmpty){
          var photos = album.photos
          if(album.coverPhoto.isDefined)
            photos = album.coverPhoto.get :: photos

          val senderRef = sender()
          for {
            photos <- (context.actorSelection(ActorNames.PhotoApiServiceRef) ? PostPhotos(photos, edgeInfo, loggedInUser)).mapTo[List[api.Photo]]
            result <- Future{
              album.coverPhoto = Some(photos.head)
              album.photos = photos
              val createdAlbum = addNewAlbum(album, edgeInfo, loggedInUser).toApiObject()
              createdAlbum.coverPhoto = Some(photos.head)
              createdAlbum.photos = photos
              senderRef ! createdAlbum
            }
          } yield result
        }

      }
    case GetAlbums(nodeId, nodeType, loggedInUser) =>
      if(!validateEdgeGetPermission(nodeId,nodeType,loggedInUser))
        sender() ! FBException("Un-Authorized")
      else {
        val albums = new ListBuffer[api.Album]()
        nodeType match {
          case NodeType.User =>
            DataStore.users.get(nodeId).foreach {
              _.albums.slice(0,20) foreach ((albumId)=> {
                val album = DataStore.albums.get(albumId)
                if(album.isDefined)
                  albums +=  album.get.toApiObject()
              })
            }

          case NodeType.Page =>
            DataStore.pages.get(nodeId).foreach {
              _.albums.slice(0,20) foreach ((albumId)=> {
                val album = DataStore.albums.get(albumId)
                if(album.isDefined)
                  albums +=  album.get.toApiObject()
              })
            }
        }
        //For each album retrieve the pictures
        albums foreach ((album) => {
          val photos = ListBuffer[api.Photo]()
          //Fill the details of photos in the Album
          album.photos foreach((photoId) =>{
            val photo = DataStore.photos.get(photoId.id.get)
            if(photo.isDefined){
              photos += photo.get.toApiObject()
              if(photo.get.encryptionInfo.isDefined) {
                //If encyrptionInfo is defined remove the encryptedSharedKey for other users while returning the data
                var encKeys = photo.get.encryptionInfo.get.encKeys
                encKeys = encKeys.filterKeys( _ == loggedInUser.id.get)
                photo.get.encryptionInfo.get.encKeys = encKeys
              }
            }
          })
          album.photos = photos.toList
          album.coverPhoto = Some(photos.head)
        })
        sender() ! api.Albums(albums.toList)
      }
  }

  override protected def getServiceManagerRef: String = ActorNames.AlbumApiServiceManagerRef

}

