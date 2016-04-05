package dos.project4.api

import dos.project4.common.{EncryptionInfo, NodeType}
import spray.http.DateTime
import spray.json._

import scala.collection.mutable

case class User(id:Option[Long],
                firstName:String,
                lastName:String,
                email:Option[String],
                birthday:Option[DateTime],
                gender:Option[String],
                pubKey:String)

case class Page(id:Option[Long],
                title:Option[String],
                description:Option[String],
                website:Option[String],
                var writtenBy:Option[Long])

case class Post(id:Option[Long],
                message:String,
                var picture:Option[Photo], //Link to a picture
                description:Option[String],
                link:Option[String], //Link associated with the post
                var edgeInfo:Option[Edge], encryptionInfo: Option[EncryptionInfo])

case class Photo(id:Option[Long],
                 link:Option[String], //Link from which the photo should be picked
                 data:Option[String],
                 var edgeInfo:Option[Edge], encryptionInfo: Option[EncryptionInfo])

case class Album(id:Option[Long],
                 var coverPhoto:Option[Photo],
                 name:Option[String],
                 var count: Option[Long],
                 var photos:List[Photo],
                 var edgeInfo:Option[Edge])

case class Edge(owner: Long, ownerNodeType:NodeType.NodeType, createdBy:Long)

case class FriendList(data:List[User])
case class Photos(data:List[Photo])
case class Posts(data:List[Post])
case class Albums(data:List[Album])

case class ObjectId(id:Long)
case class Success(status:String="Success")

case class AuthToken(user_id:Long, challenge:Long, dsig:String)
case class AuthChallenge(challenge:Long)

case class FBException(error: String)

object DefaultJsonFormatterProtocol extends DefaultJsonProtocol {
  implicit object dateFormat extends RootJsonFormat[spray.http.DateTime] {
    def read(json: JsValue): spray.http.DateTime =
      DateTime.fromIsoDateTimeString(json.convertTo[String]) getOrElse(null)
      def write(date: DateTime) = JsString(date.toIsoDateTimeString)
    }

  implicit def muSetFormat[T :JsonFormat] = viaSeq[mutable.Set[T], T](seq => mutable.Set(seq :_*))

  override implicit def mapFormat[K :JsonFormat, V :JsonFormat] = new RootJsonFormat[Map[K, V]] {
    def write(m: Map[K, V]) = JsObject {
      m.map { field =>
        field._1.toJson match {
          case JsString(x) => x -> field._2.toJson
          case JsNumber(x) => x.toString() -> field._2.toJson
          case x => throw new SerializationException("Map key must be formatted as JsString, not '" + x + "'")
        }
      }
    }
    def read(value: JsValue) = value match {
      case x: JsObject => x.fields.map { field =>{
        if(field._1.matches("[-+]?\\d+(\\.\\d+)?"))
          (JsNumber(field._1).convertTo[K], field._2.convertTo[V])
        else (JsString(field._1).convertTo[K], field._2.convertTo[V])
      }
      } (collection.breakOut)
      case x => deserializationError("Expected Map as JsObject, but got " + x)
    }
  }

  implicit object objectTypeFormat extends RootJsonFormat[NodeType.NodeType] {
    def read(json: JsValue): NodeType.NodeType = {
      var nodeType = NodeType.User
      val strValue = json.compactPrint
      if(strValue.equals("Page"))
        nodeType = NodeType.Page
      nodeType
    }
    def write(_type: NodeType.NodeType) = JsString(_type.toString)
  }

  implicit val encInfoFormat = jsonFormat2(EncryptionInfo)
  implicit val userFormat = jsonFormat7(User)
  implicit val successFormat = jsonFormat1(Success)
  implicit val edgeFormat = jsonFormat3(Edge)
  implicit val pageFormat = jsonFormat5(Page)
  implicit val photoFormat = jsonFormat5(Photo)
  implicit val postFormat = jsonFormat7(Post)
  implicit val albumFormat = jsonFormat6(Album)
  implicit val photosFormat = jsonFormat1(Photos)
  implicit val friendsFormat = jsonFormat1(FriendList)
  implicit val postsFormat = jsonFormat1(Posts)
  implicit val albumsFormat = jsonFormat1(Albums)
  implicit val objectIdFormat = jsonFormat1(ObjectId)
  implicit val authChallengeFormat = jsonFormat1(AuthChallenge)
  implicit val authTokenFormat = jsonFormat3(AuthToken)
  implicit val fbException = jsonFormat1(FBException)


}

