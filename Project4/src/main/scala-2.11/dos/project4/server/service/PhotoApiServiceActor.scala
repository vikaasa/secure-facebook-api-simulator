package dos.project4.server.service

import dos.project4._
import dos.project4.common.{ActorNames, NodeType}
import dos.project4.model.DataStore
import dos.project4.server.service.PhotoApiServiceActor.{GetPhotos, PostPhoto, PostPhotos}

import scala.collection.mutable.ListBuffer

object PhotoApiServiceActor{
  case class PostPhoto(photo:api.Photo, edgeInfo:api.Edge, loggedInUser: api.User)
  case class PostPhotos(photos:List[api.Photo], edgeInfo:api.Edge, loggedInUser: api.User)
  case class GetPhotos(nodeId:Long, nodeType:NodeType.NodeType, loggedInUser: api.User)
}



class PhotoApiServiceActor extends BaseApiServiceActor with EdgeValidationService{

  private def createPhoto(photo:api.Photo, edgeInfo:api.Edge, loggedInUser:api.User): model.Photo = {
    if(!initialised)
      getNewIdRange()
    count +=1
    photo.edgeInfo = Some(edgeInfo)
    val newPhoto = model.Photo(count, photo)
    DataStore.photos.put(count, newPhoto)
    edgeInfo.ownerNodeType match{
      case NodeType.User =>
        DataStore.users.get(edgeInfo.owner) foreach  {_.photos.prepend(newPhoto.id)}
      case NodeType.Page =>
        DataStore.pages.get(edgeInfo.owner) foreach  {_.photos.prepend(newPhoto.id)}
    }
    validateIdRange()
    newPhoto
  }
  override def receive: Receive = {
    case PostPhoto(photo, edgeInfo, loggedInUser) =>
      if(!validateEdgeCreatePermission(edgeInfo, loggedInUser)){
        sender() ! api.FBException("Un-Authorized")
      }else {
        sender() ! createPhoto(photo,edgeInfo,loggedInUser).toApiObject()
      }
    case PostPhotos(photos, edgeInfo, loggedInUser) =>
      if(!validateEdgeCreatePermission(edgeInfo, loggedInUser)){
        sender() ! api.FBException("Un-Authorized")
      } else {
        val addedPhotos = new ListBuffer[api.Photo]()
        photos foreach((photo) =>
          addedPhotos += createPhoto(photo,edgeInfo, loggedInUser).toApiObject()
          )
        sender() ! addedPhotos.toList
      }

    case GetPhotos(nodeId, nodeType, loggedInUser) =>
      if(!validateEdgeGetPermission(nodeId,nodeType, loggedInUser))
        sender() ! api.FBException("Un-Authorized")
      else {
        var photos = new ListBuffer[api.Photo]()
        nodeType match {
          case NodeType.User =>
            DataStore.users.get(nodeId) foreach { _.photos.slice(0,20) foreach ((photoId)=> {
              val photo = DataStore.photos.get(photoId)
              if(photo.isDefined)
                photos +=  photo.get.toApiObject()
            })}

          case NodeType.Page =>
            DataStore.pages.get(nodeId) foreach {_.photos.slice(0,20) foreach ((photoId)=> {
              val photo = DataStore.photos.get(photoId)
              if(photo.isDefined)
                photos +=  photo.get.toApiObject()
            })}
        }
        photos foreach ((photo)=> {
          if(photo.encryptionInfo.isDefined) {
            //If encyrptionInfo is defined remove the encryptedSharedKey for other users while returning the data
            val encKeys = photo.encryptionInfo.get.encKeys filterKeys  ( _ == loggedInUser.id.get)
            photo.encryptionInfo.get.encKeys = encKeys
          }
        })

        photos = photos.filter( (photo) => {
          if(!photo.encryptionInfo.isDefined)
            true
          else if(photo.encryptionInfo.get.encKeys.isEmpty){
            false
          } else true
        })

        sender() ! api.Photos(photos.toList)
      }
  }
  override protected def getServiceManagerRef: String = ActorNames.PhotoApiServiceManagerRef
}
