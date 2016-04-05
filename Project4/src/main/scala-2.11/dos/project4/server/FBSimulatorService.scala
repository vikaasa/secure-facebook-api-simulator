package dos.project4.server


import java.util.concurrent.TimeUnit

import akka.actor.{Actor, ActorContext}
import akka.pattern.{AskTimeoutException, ask}
import akka.util.Timeout
import dos.project4.api._
import dos.project4.common.{ActorNames, NodeType}
import dos.project4.server.service.AlbumApiServiceActor.{CreateAlbum, GetAlbums}
import dos.project4.server.service.AuthChallenger
import dos.project4.server.service.AuthServiceActor.Authenticate
import dos.project4.server.service.PageApiServiceActor.{CreatePage, GetPage}
import dos.project4.server.service.PhotoApiServiceActor.{GetPhotos, PostPhoto}
import dos.project4.server.service.PostApiServiceActor.{CreatePost, GetPosts}
import dos.project4.server.service.UserApiServiceActor._
import spray.http.MediaTypes._
import spray.httpx.SprayJsonSupport._
import spray.json.JsonParser
import spray.routing.AuthenticationFailedRejection.CredentialsRejected
import spray.routing._
import spray.routing.authentication._
import spray.util.LoggingContext

import scala.concurrent.Future



class FBSimulatorServiceActor extends Actor with FBSimulatorService {

  // the HttpService trait defines only one abstract member, which
  // connects the services environment to the enclosing actor or test
  def actorRefFactory = context

  // this actor only runs our route, but you could add
  // other things here, like request stream processing
  // or timeout handling
  def receive = runRoute(authChallengeRouter ~ userRouter ~ pageRouter ~ bootStrapRouter )


}



// this trait defines our service behavior independently from the service actor
trait FBSimulatorService extends HttpService with CustomUserAuthentication {

  implicit val ec = actorRefFactory.dispatcher
  implicit val timeout = Timeout(10, TimeUnit.SECONDS)
  import dos.project4.api.DefaultJsonFormatterProtocol._

  implicit def exceptionHandler(implicit log: LoggingContext) =
    ExceptionHandler {
      case e: AskTimeoutException =>
        complete(FBException("Ask Timeout"))
    }

  val authChallengeRouter = {
    path("auth"){
      get{
        respondWithMediaType(`application/json`) {
          complete(AuthChallenger.getNextChallenge)
        }
      }
    }
  }

  val bootStrapRouter = {
    path("bootstrap" / "avgfriends" / IntNumber){(avgFriends) =>
      get{
        respondWithMediaType(`application/json`) {
          onSuccess((actorRefFactory.actorSelection(ActorNames.UserApiServiceRef) ? BootStrapFriendList(avgFriends))(Timeout(120, TimeUnit.SECONDS))){
            case result:dos.project4.api.Success =>
              complete(result)
            case ex:FBException =>
              respondWithStatus(400){
                complete(ex)
              }
          }
        }
      }

    }

  }

  val userRouter = {
    path("user") {
      put {
        entity(as[User]) { user =>
          respondWithMediaType(`application/json`) {
            onSuccess(actorRefFactory.actorSelection(ActorNames.UserApiServiceRef) ? CreateUser(user)){
              case result:User =>
                complete(result)
              case ex:FBException =>
                respondWithStatus(400){
                  complete(ex)
                }
            }
          }
        }
      } ~ post {
        authenticate(customAuthn) { user =>
          entity(as[User]) { updatedUser =>
            respondWithMediaType(`application/json`) {
              onSuccess(actorRefFactory.actorSelection(ActorNames.UserApiServiceRef) ? UpdateUser(updatedUser, user)) {
                case result:User =>
                  complete(result)
                case ex: FBException =>
                  respondWithStatus(400) {
                    complete(ex)
                  }
              }
            }
          }
        }
      }
    } ~ authenticate(customAuthn) { user =>
      pathPrefix("user" / LongNumber) { (id) =>
        pathEnd {
          get {
            respondWithMediaType(`application/json`) {
              onSuccess(actorRefFactory.actorSelection(ActorNames.UserApiServiceRef) ? GetUser(id, user)) {
                case result: User =>
                  complete(result)
                case ex: FBException =>
                  respondWithStatus(401) {
                    complete(ex)
                  }
              }
            }
          }
        }
      } ~ path("user"/ LongNumber / "friends") { (id) =>
        put {
          entity(as[ObjectId]) { friendId =>
            respondWithMediaType(`application/json`) {
              onSuccess(actorRefFactory.actorSelection(ActorNames.UserApiServiceRef) ? AddFriend(friendId.id, id, user)){
                case result:Success =>
                  complete(result)
                case ex:FBException =>
                  respondWithStatus(400){
                    complete(ex)
                  }
              }
            }
          }
        } ~ get {
          respondWithMediaType(`application/json`) {
            onSuccess(actorRefFactory.actorSelection(ActorNames.UserApiServiceRef) ? GetFriends(id, user)){
              case result:FriendList =>
                complete(result)
              case ex:FBException =>
                respondWithStatus(400){
                  complete(ex)
                }
            }
          }
        }
      } ~ path("user" / LongNumber / "posts") { (id) =>
        put{
          entity(as[Post]) { post =>
            onSuccess(actorRefFactory.actorSelection(ActorNames.PostApiServiceRef) ? CreatePost(post, Edge(id, NodeType.User, user.id.get), user)) {
              case result: Post =>
                complete(result)
              case ex: FBException =>
                respondWithStatus(400) {
                  complete(ex)
                }
            }
          }
        } ~ get {
          onSuccess(actorRefFactory.actorSelection(ActorNames.PostApiServiceRef) ? GetPosts(id, NodeType.User, user)){
            case result:Posts =>
              complete(result)
            case ex:FBException =>
              respondWithStatus(400){
                complete(ex)
              }
          }
        }
      } ~ path("user" / LongNumber / "photos") { (id) =>
        put{
          entity(as[Photo]) { photo =>
            onSuccess(actorRefFactory.actorSelection(ActorNames.PhotoApiServiceRef) ? PostPhoto(photo, Edge(id, NodeType.User, user.id.get), user)) {
              case result: Photo =>
                complete(result)
              case ex: FBException =>
                respondWithStatus(400) {
                  complete(ex)
                }
            }
          }
        } ~ get {
          onSuccess(actorRefFactory.actorSelection(ActorNames.PhotoApiServiceRef) ? GetPhotos(id, NodeType.User, user)){
            case result:Photos =>
              complete(result)
            case ex:FBException =>
              respondWithStatus(400){
                complete(ex)
              }
          }
        }
      } ~ path("user" /LongNumber / "albums") { (id) =>
        put{
          entity(as[Album]) { album =>
            onSuccess(actorRefFactory.actorSelection(ActorNames.AlbumApiServiceRef) ? CreateAlbum(album, Edge(id, NodeType.User, user.id.get), user)) {
              case result: Album =>
                complete(result)
              case ex: FBException =>
                respondWithStatus(400) {
                  complete(ex)
                }
            }
          }
        } ~ get {
          onSuccess(actorRefFactory.actorSelection(ActorNames.AlbumApiServiceRef) ? GetAlbums(id, NodeType.User, user)){
            case result:Albums =>
              complete(result)
            case ex:FBException =>
              respondWithStatus(400){
                complete(ex)
              }
          }
        }
      }
    }
  }

  val pageRouter = {
    authenticate(customAuthn) { user =>
      respondWithMediaType(`application/json`){
        path("page") {
          put {
            entity(as[Page]) { page =>
              onSuccess(actorRefFactory.actorSelection(ActorNames.PageApiServiceRef) ? CreatePage(page, user)) {
                case result: Page =>
                  complete(result)
                case ex: FBException =>
                  respondWithStatus(400) {
                    complete(ex)
                  }
              }
            }
          }
        } ~ pathPrefix("page" / LongNumber) { (id) =>
          pathEnd {
            get {
              onSuccess(actorRefFactory.actorSelection(ActorNames.PageApiServiceRef) ? GetPage(id)) {
                case result: Page =>
                  complete(result)
                case ex:FBException => respondWithStatus(400) {
                  complete(ex)
                }
              }
            }
          }
        } ~ path("page" / LongNumber / "posts") { (id) =>
          put{
            entity(as[Post]) { post =>
              onSuccess(actorRefFactory.actorSelection(ActorNames.PostApiServiceRef) ? CreatePost(post, Edge(id, NodeType.Page, user.id.get), user)) {
                case result: Post =>
                  complete(result)
                case ex: FBException =>
                  respondWithStatus(400) {
                    complete(ex)
                  }
              }
            }
          } ~ get {
            onSuccess(actorRefFactory.actorSelection(ActorNames.PostApiServiceRef) ? GetPosts(id, NodeType.Page, user)){
              case result:Posts =>
                complete(result)
              case ex:FBException =>
                respondWithStatus(400){
                  complete(ex)
                }
            }
          }
        } ~ path("page" / LongNumber / "photos") { (id) =>
          put{
            entity(as[Photo]) { photo =>
              onSuccess(actorRefFactory.actorSelection(ActorNames.PhotoApiServiceRef) ? PostPhoto(photo, Edge(id, NodeType.Page, user.id.get), user)) {
                case result: Photo =>
                  complete(result)
                case ex: FBException =>
                  respondWithStatus(400) {
                    complete(ex)
                  }
              }
            }
          } ~ get {
            onSuccess(actorRefFactory.actorSelection(ActorNames.PhotoApiServiceRef) ? GetPhotos(id, NodeType.Page, user)){
              case result:Photos =>
                complete(result)
              case ex:FBException =>
                respondWithStatus(400){
                  complete(ex)
                }
            }
          }
        } ~ path("page" /LongNumber / "albums") { (id) =>
          put{
            entity(as[Album]) { album =>
              onSuccess(actorRefFactory.actorSelection(ActorNames.AlbumApiServiceRef) ? CreateAlbum(album, Edge(id, NodeType.Page, user.id.get), user)) {
                case result: Album =>
                  complete(result)
                case ex: FBException =>
                  respondWithStatus(400) {
                    complete(ex)
                  }
              }
            }
          } ~ get {
            onSuccess(actorRefFactory.actorSelection(ActorNames.AlbumApiServiceRef) ? GetAlbums(id, NodeType.Page, user)){
              case result:Albums =>
                complete(result)
              case ex:FBException =>
                respondWithStatus(400){
                  complete(ex)
                }
            }
          }
        }
      }
    }
  }






}


trait CustomUserAuthentication {

  import dos.project4.api.DefaultJsonFormatterProtocol._
  def actorRefFactory: ActorContext
  def customAuthn: ContextAuthenticator[User] = ctx =>
  {
    val headers = ctx.request.headers
    val authHeaders = headers.filter( _.name == "X-Auth")
    if(authHeaders.size != 0){
      val json = JsonParser(authHeaders.head.value)
      val authToken = json.convertTo[AuthToken]


      doAuth(authToken, ctx.request.uri.toString, ctx.request.entity.asString )
    }
    else doAuth(null, null, null)
  }

  private def doAuth(authToken: AuthToken, uri: String, content : String): Future[Authentication[User]] = {
    implicit val ec = actorRefFactory.dispatcher
    implicit val timeout = Timeout(10, TimeUnit.SECONDS)

    if(authToken!= null){
      val f = for {
        a <- (actorRefFactory.actorSelection(ActorNames.AuthServiceRef) ? Authenticate(authToken, uri, content)).mapTo[Option[User]]
        result <- Future[Authentication[User]] {
        a match {
          case Some(user:User) =>
            Either.cond(true,user, AuthenticationFailedRejection(CredentialsRejected, List()))
          case None => Either.cond(false, null, AuthenticationFailedRejection(CredentialsRejected, List()))
        }
      }
      } yield result
      f
    }
    else Future{Either.cond(false, null, AuthenticationFailedRejection(CredentialsRejected, List()))}

  }
}

