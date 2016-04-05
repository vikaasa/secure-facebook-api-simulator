package dos.project4.client

import java.util.concurrent.TimeUnit

import akka.actor.Actor
import akka.util.Timeout
import dos.project4.client.FakeUser._
import dos.project4.client.FakeUserCoordinator.Start
import dos.project4.common.ActorNames

import scala.concurrent.duration._
import scala.util.Random

object EdgeOperation extends Enumeration {
  type EdgeOperation = Value
  val PostOp, PhotoOp, AlbumOp = Value
}
trait EdgeOperationRoller {

  def rollOperation(): EdgeOperation.EdgeOperation = {
    val rolledValue = Random.nextInt(100) + 1
    if(rolledValue<=ClientConfig.photoOperationRollerLimit)
      EdgeOperation.PhotoOp
    else if(rolledValue <= ClientConfig.postOperationRollerLimit)
      EdgeOperation.PostOp
    else EdgeOperation.AlbumOp
  }

}

class CreateOpTickManager extends Actor with EdgeOperationRoller{
  implicit val ec = context.dispatcher
  implicit val timeout = Timeout(1, TimeUnit.SECONDS)
  override def receive: Receive ={
    case Start =>
      val ticker_rate = ClientConfig.create_ticker_rate
      val microsecond_tick_interval = (1/ticker_rate * 10E6).toLong
      context.system.scheduler.schedule(Duration.create(0, TimeUnit.SECONDS), Duration.create(microsecond_tick_interval, TimeUnit.MICROSECONDS), new Runnable {
        override def run(): Unit = {
          if(rollOperation() == EdgeOperation.PhotoOp){
            if(Random.nextBoolean())
              context.actorSelection(ActorNames.FakeUserPrefixRef+Random.nextInt(ClientConfig.numUsers)) ! CreatePostWithPhoto
            else context.actorSelection(ActorNames.FakeUserPrefixRef+Random.nextInt(ClientConfig.numUsers)) ! PostPhoto
          } else if (rollOperation() == EdgeOperation.PostOp)
            context.actorSelection(ActorNames.FakeUserPrefixRef+Random.nextInt(ClientConfig.numUsers)) ! CreatePost
          else context.actorSelection(ActorNames.FakeUserPrefixRef+Random.nextInt(ClientConfig.numUsers)) ! CreateAlbum
        }
      })
  }
}

class GetOpTickManager extends Actor with EdgeOperationRoller{
  implicit val ec = context.dispatcher
  implicit val timeout = Timeout(1, TimeUnit.SECONDS)
  override def receive: Receive ={
    case Start =>
      val ticker_rate = ClientConfig.get_ticker_rate
      val microsecond_tick_interval = (1/ticker_rate * 10E6).toLong
      context.system.scheduler.schedule(Duration.create(0, TimeUnit.SECONDS), Duration.create(microsecond_tick_interval, TimeUnit.MICROSECONDS), new Runnable {
        override def run(): Unit = {
          if(rollOperation() == EdgeOperation.PhotoOp){
            context.actorSelection(ActorNames.FakeUserPrefixRef+Random.nextInt(ClientConfig.numUsers)) ! GetPhotos
          } else if (rollOperation() == EdgeOperation.PostOp)
            context.actorSelection(ActorNames.FakeUserPrefixRef+Random.nextInt(ClientConfig.numUsers)) ! GetPosts
          else context.actorSelection(ActorNames.FakeUserPrefixRef+Random.nextInt(ClientConfig.numUsers)) ! GetAlbums
        }
      })
  }
}
