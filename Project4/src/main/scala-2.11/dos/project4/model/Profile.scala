package dos.project4.model

import scala.collection.mutable



abstract class Profile(override val id:Long) extends FBObject(id){
  var posts = new mutable.ListBuffer[Long]
  var photos = new mutable.ListBuffer[Long]
  var albums = new mutable.ListBuffer[Long]
}
