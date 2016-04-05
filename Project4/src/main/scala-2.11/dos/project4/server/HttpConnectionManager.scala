package dos.project4.server

import java.net.InetAddress
import java.util.concurrent.TimeUnit

import akka.actor.{Actor, ActorRef}
import akka.io.IO
import akka.util.Timeout
import dos.project4.server.HttpConnectionManager.{RespondedRequest, ShowStats, Start}
import spray.can.Http
import spray.can.server.Stats

import scala.concurrent.duration.Duration

object HttpConnectionManager{
  case class Start(service:ActorRef)
  case class ShowStats()
  case class ReceivedRequest()
  case class RespondedRequest()

}
class HttpConnectionManager extends Actor{
  implicit val ec = context.dispatcher
  implicit val timeout = Timeout(5, TimeUnit.SECONDS)
  implicit val system = context.system
  var httpListenerRef: ActorRef = null
  var receivedRequests:Int = 0
  var respondedRequests:Int = 0
  override def receive: Receive = {
    case Start(service) =>
      IO(Http) ! Http.Bind(service, interface = "0.0.0.0", port = 8080) // Bind on all ports
    case x:Http.Bound =>
      httpListenerRef = sender()
      context.system.scheduler.schedule(Duration.create(0, TimeUnit.SECONDS), Duration.create(1, TimeUnit.SECONDS),self, ShowStats)
    case x: Stats =>
      if( x.totalRequests != 0)
        println("Received Requests: " + x.totalRequests +  ", Requests still under processing: "+ x.openRequests )
      httpListenerRef ! Http.ClearStats
      respondedRequests = 0
    case ShowStats =>
      httpListenerRef ! Http.GetStats
    case RespondedRequest =>
      respondedRequests +=1
  }

}
