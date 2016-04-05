package dos.project4.server.service

import java.security.SecureRandom
import java.util.concurrent.TimeUnit


import com.twitter.storehaus.cache.MutableTTLCache
import dos.project4.api.AuthChallenge

import scala.collection.concurrent.TrieMap

object AuthChallenger{

  val sRNG = SecureRandom.getInstance("SHA1PRNG")
  sRNG.setSeed(sRNG.generateSeed(32))

  val cache = MutableTTLCache[Long, Long](com.twitter.util.Duration(10, TimeUnit.SECONDS),1000000)
  def getNextChallenge:AuthChallenge = {
    synchronized{
      var c = 0L
      do {
        c = sRNG.nextLong()
      }while (containsChallenge(c))
      cache += (c -> 0)
      AuthChallenge(c)
    }

  }
  def challengeComplete(challenge:Long):Boolean = {
    synchronized {
      var ret = false
      val a = cache.get(challenge)
      if(a.isDefined && a.get <=5){
        cache += (challenge -> (a.get+1))
        ret = true
      } else {
        ret = cache.evict(challenge).isDefined
      }
      ret
    }
  }
  def containsChallenge(challenge:Long): Boolean = cache.contains(challenge)


}




