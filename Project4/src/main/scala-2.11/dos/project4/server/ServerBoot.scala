package dos.project4.server

import akka.actor.{ActorSystem, Props}
import akka.routing.FromConfig
import dos.project4.common.ActorNames
import dos.project4.server.HttpConnectionManager.Start
import dos.project4.server.service._

object ServerBoot extends App {


  // we need an ActorSystem to host our application in
  implicit val system = ActorSystem("FB_simulator_server")

  // create and start our service actor
  val service = system.actorOf(Props[FBSimulatorServiceActor], ActorNames.FBSimulatorService)


  val user_api_manager = system.actorOf(Props[ApiServiceManager], ActorNames.UserApiServiceManager)
  val auth_api_manager  = system.actorOf(Props[ApiServiceManager], ActorNames.AuthApiServiceManager)
  val page_api_manager  = system.actorOf(Props[ApiServiceManager], ActorNames.PageApiServiceManager)
  val post_api_manager  = system.actorOf(Props[ApiServiceManager], ActorNames.PostApiServiceManager)
  val photo_api_manager  = system.actorOf(Props[ApiServiceManager], ActorNames.PhotoApiServiceManager)
  val album_api_manager  = system.actorOf(Props[ApiServiceManager], ActorNames.AlbumApiServiceManager)

  val user_api = system.actorOf(FromConfig.props(Props[UserApiServiceActor]), ActorNames.UserApiService)
  val auth_api = system.actorOf(FromConfig.props(Props[AuthServiceActor]), ActorNames.AuthService)
  val page_api = system.actorOf(FromConfig.props(Props[PageApiServiceActor]), ActorNames.PageApiService)
  val post_api = system.actorOf(FromConfig.props(Props[PostApiServiceActor]), ActorNames.PostApiService)
  val photo_api = system.actorOf(FromConfig.props(Props[PhotoApiServiceActor]), ActorNames.PhotoApiService)
  val album_api = system.actorOf(FromConfig.props(Props[AlbumApiServiceActor]), ActorNames.AlbumApiService)


  val httpConnectionManager = system.actorOf(Props[HttpConnectionManager], ActorNames.HttpConnectionManager)
  httpConnectionManager ! Start(service)




}


