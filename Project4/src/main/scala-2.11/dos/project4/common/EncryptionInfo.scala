package dos.project4.common

case class EncryptionInfo(var encKeys:Map[Long, String], initVector: String)