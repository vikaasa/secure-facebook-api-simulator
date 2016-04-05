package dos.project4.server.service

import java.util.concurrent.TimeUnit

import akka.actor.Actor
import akka.pattern.ask
import akka.util.Timeout
import dos.project4.server.service.ApiServiceManager.{GetRange, IdsRange}

import scala.concurrent.Await
import scala.concurrent.duration.Duration

abstract class BaseApiServiceActor extends Actor {
  protected implicit val ec = context.dispatcher
  protected implicit val timeout = Timeout(10, TimeUnit.SECONDS)

  protected var count = 0L
  protected var countLimit = 0L
  protected var initialised = false

  protected def validateIdRange(): Unit = {
    if(count == countLimit)
      getNewIdRange
  }

  protected def getNewIdRange(): Unit = {
    val range = Await.result(context.actorSelection(getServiceManagerRef) ? GetRange,
      Duration.create(10,TimeUnit.SECONDS)).asInstanceOf[IdsRange]
    count = range.start - 1
    countLimit = range.end

    initialised = true
  }

  protected def getServiceManagerRef: String
}
