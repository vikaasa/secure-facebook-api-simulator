package dos.project4.model


import scala.collection.concurrent.{Map, TrieMap}

object DataStore {
  val users:Map[Long, User]= new TrieMap[Long, User]()
  val posts:Map[Long, Post]= new TrieMap[Long, Post]()
  val photos:Map[Long, Photo]= new TrieMap[Long, Photo]()
  val albums:Map[Long, Album]= new TrieMap[Long, Album]()
  val pages:Map[Long, Page]= new TrieMap[Long, Page]()
}
