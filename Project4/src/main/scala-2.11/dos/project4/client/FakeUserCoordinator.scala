package dos.project4.client

import java.util.concurrent.TimeUnit

import akka.actor.{Actor, Cancellable, Props}
import akka.util.Timeout
import dos.project4.client.FakeUser._
import dos.project4.client.FakeUserCoordinator._
import dos.project4.common.ActorNames

import scala.concurrent.duration.Duration
import scala.util.Random

object FakeUserCoordinator{
  case class Start()
  case class StartSimulation()
  case class RequestComplete()
}

class FakeUserCoordinator extends Actor{

  implicit val ec = context.dispatcher
  implicit val timeout = Timeout(120, TimeUnit.SECONDS)

  var totalFriendConnections = 0L
  var totalFriendConnectionsComplete = 0L
  var friendListInitCount = 0L

  var startTime = 0L
  var endTime = 0L


  var numUserRequestsComplete = 0L
  val createTicker:Cancellable = null
  val getTicker:Cancellable = null


  def signUp: Receive = {
    case Start =>
      startTime = System.currentTimeMillis()
      for(i <- 0L until ClientConfig.numUsers){
        context.actorSelection(ActorNames.FakeUserPrefixRef+i) ! SignUp
      }
    case RequestComplete =>
      numUserRequestsComplete += 1
      //Start adding friends to reach the avg friends ratio
      if(numUserRequestsComplete == ClientConfig.numUsers) {
        println("All User SignUp Complete, time taken: " + (System.currentTimeMillis() - startTime) / 1000.0)
        numUserRequestsComplete = 0
        context.become(addFriends)
        self ! Start
      }
  }

  def addFriends:Receive = {
    case Start =>
      startTime = System.currentTimeMillis()
      //Perform add friends
      context.actorSelection(ActorNames.FakeUserPrefixRef+0) ! BootStartFriendList(ClientConfig.avgFriends)
      context.system.scheduler.scheduleOnce(Duration.create(60,TimeUnit.SECONDS),self, RequestComplete)
    case RequestComplete =>
      println("Random FriendList creation done, time taken: " + (System.currentTimeMillis() - startTime) / 1000.0)
      numUserRequestsComplete = 0
      context.become(initFriends)
      self ! Start
  }

  def initFriends:Receive = {
    case Start =>
      startTime = System.currentTimeMillis()
      for(i <- 0L until ClientConfig.numUsers){
        context.actorSelection(ActorNames.FakeUserPrefixRef+i) ! InitUserFriends
      }
    case RequestComplete =>
      numUserRequestsComplete += 1
      if(numUserRequestsComplete == ClientConfig.numUsers) {
        numUserRequestsComplete = 0
        println("All Users Initialized Friends, time taken: " + (System.currentTimeMillis() - startTime) / 1000.0)
        context.become(initPages)
        self ! Start
      }
  }

  def initPages: Receive = {
    case Start =>
      startTime = System.currentTimeMillis()
      for(i <- 0L until ClientConfig.numPages){
        context.actorSelection(ActorNames.FakeUserPrefixRef+i) ! CreateAPage
      }
    case RequestComplete =>
      numUserRequestsComplete += 1
      if(numUserRequestsComplete == ClientConfig.numPages) {
        numUserRequestsComplete = 0
        println("All Pages Initialized, time taken: " + (System.currentTimeMillis() - startTime) / 1000.0)
//        context.become(runSimulation)
//        self ! Start
        context.become(demoSecurity)
        self ! Start
      }
  }

  def demoSecurity:Receive = {
    case Start =>
      println("Starting Demo flow..")
      context.actorSelection(ActorNames.FakeUserPrefixRef+Random.nextInt(ClientConfig.numUsers)) ! PerformDemoFlow
//      context.system.scheduler.scheduleOnce(Duration.create(10, TimeUnit.SECONDS), new Runnable(){
//        override def run(): Unit = {
//          println("Demo flow complete..exiting")
//          System.exit(0)
//        }
//      })
  }

  def runSimulation: Receive = {
    case Start =>
      println("All bootstrapping done, starting simulation now. ")
      println()
      println()
      println("Running Simulation now, sit back and relax!!!")
      val create_op_tick = context.system.actorOf(Props[CreateOpTickManager], ActorNames.CreateOpTickManager)
      val get_op_tick = context.system.actorOf(Props[GetOpTickManager], ActorNames.GetOpTickManager)

      create_op_tick ! Start
      get_op_tick ! Start

//      context.system.scheduler.scheduleOnce(Duration.create(ClientConfig.simulationTime, TimeUnit.SECONDS), new Runnable(){
//        override def run(): Unit = {
//          println("Simulation complete")
//          context.become(demoSecurity)
//          self ! Start
//        }
//      })
  }

  override def receive: Receive = {
    case StartSimulation =>
      context.become(signUp)
      self ! Start
  }
}
