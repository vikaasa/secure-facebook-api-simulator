package dos.project4.server.service

import akka.actor.Actor
import dos.project4.server.service.ApiServiceManager.{GetRange, IdsRange}

object ApiServiceManager{
  case class GetRange()
  case class IdsRange(start:Long, end:Long)
}
class ApiServiceManager extends Actor{
  var startRange = 1
  val rangeLimit = 10000
  override def receive: Actor.Receive = {
    case GetRange =>
      val start = startRange
      startRange += rangeLimit + 1
      sender() ! IdsRange(start, start + rangeLimit)
  }
}