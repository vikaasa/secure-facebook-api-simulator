package dos.project4.client

import java.io.FileInputStream
import java.security.KeyStore
import java.util
import java.util.Collections

import akka.actor.{ActorSystem, Props}
import dos.project4.client.FakeUserCoordinator.StartSimulation
import dos.project4.common.ActorNames

import scala.collection.concurrent.TrieMap

object ClientConfig {
  var numUsers = 10000
  def numPages = (0.2 * numUsers).toInt
  def create_ticker_rate = (create_ticker_rate_10000 * numUsers)/10000
  def get_ticker_rate = (get_ticker_rate_10000 * numUsers)/10000


  def avgFriends:Int = {
    if(numUsers >= 10 && numUsers <=100)
      5
    else if(numUsers>100 && numUsers <=1000)
      10
    else if (numUsers> 1000 && numUsers < 10000)
      20
    else 338 // actual average friend stats obtained from facebook.
  }

  val pageOperationsWrtUsers = 5
  val create_ticker_rate_10000 = 1000
  val get_ticker_rate_10000 = 4000
  val pageIds = Collections.synchronizedList(new util.ArrayList[Long]())
  val photoOperationRollerLimit = 75 //75%
  val postOperationRollerLimit = 96 //21%
  var simulationTime = 10
  var serverIp = "localhost"
  var keystore:KeyStore = null
  var userIdActorNumberMapping = new TrieMap[Long, Long]
}

object ClientSimulator extends App{
  if(args.length < 2) {
    println("Invalid Arguments")
    showUsage()
  } else {
    try{
      val numUsers = args(0).toInt
      val simulationTime = args(1).toInt

      ClientConfig.numUsers = numUsers // Set the arguments in the config singleton class
      ClientConfig.simulationTime = simulationTime
      if(args.length == 3) {
        ClientConfig.serverIp = args(2)
      }

      try {
        val storepass = "storepass".toCharArray
        val keypass = "keypass".toCharArray
        val startTime = System.currentTimeMillis()

        val ks = KeyStore.getInstance("jceks")
        val fis = new FileInputStream("testKS.jck")

        ks.load(fis, storepass)
        ClientConfig.keystore = ks
      } catch {
        case ex:Throwable => println(ex)
      }

      implicit val system = ActorSystem("FB_fake_client_simulator")
      val fakeUserCO = system.actorOf(Props[FakeUserCoordinator], ActorNames.FakeUserCoOrdinator)
      for(i <-0 until ClientConfig.numUsers) {
        system.actorOf(Props[FakeUser], ActorNames.FakeUserPrefix +i)
      }
      println("All Fake Client actors created")
      fakeUserCO ! StartSimulation

    }catch{
      case e: NumberFormatException =>
        println("Error: numNodes and simulationTime must be an Integer")
        showUsage()
        System.exit(1)
    }

  }

  def showUsage():Unit = {
    println("\tUsage:")
    println("\t\tsbt \"run numUsers simulationTime [server_ip]\"")
  }

}
