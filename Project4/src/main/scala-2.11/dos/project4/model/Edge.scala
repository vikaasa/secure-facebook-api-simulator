package dos.project4.model

import dos.project4.common.NodeType

abstract class Edge(override val id:Long) extends FBObject(id){
  val owner:Long
  val createdBy:Long
  val ownerNodeType:NodeType.NodeType
}
