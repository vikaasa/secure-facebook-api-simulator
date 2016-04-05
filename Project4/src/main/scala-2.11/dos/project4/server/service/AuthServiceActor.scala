package dos.project4.server.service

import java.util.concurrent.TimeUnit

import akka.actor.Actor
import akka.util.Timeout
import dos.project4.api.AuthToken
import dos.project4.common.Crypto
import dos.project4.model.DataStore
import dos.project4.server.service.AuthServiceActor.Authenticate

object AuthServiceActor{
  case class Authenticate(authToken: AuthToken, url:String, content:String)
}

class AuthServiceActor extends Actor{
  implicit val ec = context.dispatcher
  implicit val timeout = Timeout(10, TimeUnit.SECONDS)
  override def receive: Receive = {
    case Authenticate(authToken, url, content) =>
      if(DataStore.users.get(authToken.user_id).isDefined)
      {
        val user = DataStore.users.get(authToken.user_id).get
        if(AuthChallenger.challengeComplete(authToken.challenge)){
          val signedContent = authToken.user_id + ":" + authToken.challenge + ":" + url + ":"+ content
          if(Crypto.verify(signedContent, authToken.dsig, Crypto.toRSAPublicKey(user.pubKey))) {
            sender () ! Some(user.toApiObject())
          } else {
            println("Signature verificaiton failed")
            sender () ! None
          }
        } else {
          println("Invalid Challenge: " + authToken.challenge )
          sender() ! None
        }
      } else {
        sender() ! None
        println("User not found")
      }


  }
}
