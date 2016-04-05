package dos.project4.client

import java.security.{PrivateKey, PublicKey}
import java.util.concurrent.TimeUnit

import akka.actor.Actor
import akka.util.Timeout
import dos.project4._
import dos.project4.api._
import dos.project4.client.FakeUser._
import dos.project4.client.FakeUserCoordinator._
import dos.project4.common.{ActorNames, Crypto, EncryptionInfo}
import spray.httpx.SprayJsonSupport
import spray.json.{JsonParser, JsonReader, pimpAny}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.util.Random


object FakeUser{

  case class GetProfile()
  case class GetPosts()
  case class GetPhotos()
  case class GetAlbums()

  case class PerformDemoFlow()
  case class DemoEncryption()

  case class SignUp()
  case class AddFriend()
  case class InitUserFriends()

  case class CreatePost()
  case class CreatePostWithPhoto()
  case class CreatePostWithPhotoAllFriendsShare()
  case class PostPhoto()
  case class CreateAlbum()

  case class CreateAPage()
  case class PingQuery()

  case class BootStartFriendList(avgFriends:Int)

}


class FakeUser() extends Actor with SprayJsonSupport{


  import DefaultJsonFormatterProtocol._
  import spray.client.pipelining._
  import spray.http._

  var userId = 0L
  val waitTime = 50
  val urlPrefix = "http://"+ClientConfig.serverIp+ ":8080"
  val userPrefix = urlPrefix + "/user"
  val pagePrefix = urlPrefix + "/page"
  val postsSuffix = "/posts"
  val photosSuffix = "/photos"
  val friendsSuffix = "/friends"
  val albumsSuffix = "/albums"
  val authPrefix = urlPrefix + "/auth"
  var friendIds : IndexedSeq[Long] = null
//  var friendPubKeys:Map[Long, PublicKey] = null
  var privateKey:PrivateKey = null
  var publicKey:PublicKey = null

  val actorSuffix = self.path.name.split(":")(1)

  implicit val ec = context.dispatcher
  implicit val timeout = Timeout(120, TimeUnit.SECONDS)




//  var pipeline: HttpRequest => Future[HttpResponse] =  null

  val defaultRespHdlr : HttpResponse => Unit = (httpResponse) =>
    if(httpResponse.status.isFailure) {
      println("Failed: "+ httpResponse.entity.asString)
    }


  val decryptRespHdlr : HttpResponse => Unit = (httpResponse) => {
    if(httpResponse.status.isFailure) {
      println("Failed: "+ httpResponse.entity.asString)
    } else {
      println("content:" + httpResponse.entity.asString)
    }

  }


  private def restUrlBuilder(prefix:String, nodeId:Long, suffix:Option[String]): String = {
    val suffixVal = suffix match {
      case Some(suffixStr) => suffixStr
      case None => ""
    }
    prefix + "/" + nodeId + suffixVal.trim
  }

  private def pageOrUserRoller():Boolean = {
    (Random.nextInt(100) + 1) <= ClientConfig.pageOperationsWrtUsers
  }

  private def getRandomFriendIds: List[Long]={
    val randomFriends = new mutable.HashSet[Long]
    randomFriends += userId
    if(friendIds.size> 1){
      val numFriends = Random.nextInt(friendIds.size-1) + 1
      do {
        var friendId = 0L
        do{
          friendId = friendIds(Random.nextInt(friendIds.size))
        }while(randomFriends.contains(friendId))
        randomFriends += friendId
      }while (randomFriends.size < numFriends)

      randomFriends.toList
    }
    else friendIds.toList

  }

  private def getFriendPublicKey(friendId:Long): PublicKey = {
//    println(ClientConfig.actorSuffixUserIdMapping(friendId))
    ClientConfig.keystore.getCertificate("pk"+ClientConfig.userIdActorNumberMapping(friendId)).getPublicKey
  }

  private def encryptPost(post: api.Post, friendIds: List[Long]):api.Post ={
    val secretKey = Crypto.generateAESKey()
    val initVector = Crypto.generateInitVector()
    val encryptedKeys = new mutable.HashMap[Long, String]
    encryptedKeys.put(userId, Crypto.encryptSharedKey(secretKey, publicKey))
    friendIds foreach ((friendId)=>{
      encryptedKeys.put(friendId, Crypto.encryptSharedKey(secretKey, getFriendPublicKey(friendId)))
    })

    //Encrypt the fields now
    val message = Crypto.encrypt(post.message,secretKey, initVector)
    val photo = post.picture map (encryptPhoto(_, friendIds))
    val description =  post.description map (Crypto.encrypt(_,secretKey, initVector))
    val link = post.link map (Crypto.encrypt(_,secretKey, initVector))
    val encryptionInfo = EncryptionInfo(encryptedKeys.toMap, Crypto.encode(initVector))
    if(!encryptedKeys.contains(userId))
      println("Alarm!!!!")
    api.Post(None,message,photo,description, link,None, Some(encryptionInfo))
  }

  private def decryptPost(post: api.Post): api.Post ={
    if(post.encryptionInfo.isDefined){
      val encKey = post.encryptionInfo.get.encKeys.get(userId)
      if(encKey.isDefined){
        val secretKey = Crypto.decryptSharedKey(encKey.get, privateKey)
        val initVector = Crypto.decode(post.encryptionInfo.get.initVector)
        val message = Crypto.decrypt(post.message,secretKey, initVector)
        val photo = post.picture map (decryptPhoto(_))
        val description =  post.description map (Crypto.decrypt(_,secretKey, initVector))
        val link = post.link map (Crypto.decrypt(_,secretKey, initVector))
        api.Post(post.id,message,photo,description, link,post.edgeInfo, None)
      } else post
    } else post
  }



  private def encryptPhoto(photo: Photo, friendIds: List[Long]): Photo = {
    val secretKey = Crypto.generateAESKey()
    val initVector = Crypto.generateInitVector()
    val encryptedKeys = new mutable.HashMap[Long, String]
    encryptedKeys.put(userId, Crypto.encryptSharedKey(secretKey, publicKey))
    friendIds foreach ((friendId)=>{
      encryptedKeys.put(friendId, Crypto.encryptSharedKey(secretKey, getFriendPublicKey(friendId)))
    })
    val encryptionInfo = EncryptionInfo(encryptedKeys.toMap, Crypto.encode(initVector))
    if(!encryptedKeys.contains(userId))
      println("Alarm!!!!")
    val photolink = photo.link map (Crypto.encrypt(_,secretKey, initVector))
    val data = photo.data map (Crypto.encrypt(_,secretKey, initVector))
    Photo(None,photolink, data, None, Some(encryptionInfo))
  }

  private def decryptPhoto(photo: Photo) : Photo = {
    if(photo.encryptionInfo.isDefined){
      val encKey = photo.encryptionInfo.get.encKeys.get(userId)
      if(encKey.isDefined){
        val secretKey = Crypto.decryptSharedKey(encKey.get, privateKey)
        val initVector = Crypto.decode(photo.encryptionInfo.get.initVector)
        val photolink = photo.link map (Crypto.decrypt(_,secretKey, initVector))
        val data = photo.data map (Crypto.decrypt(_,secretKey, initVector))
        Photo(photo.id,photolink, data, photo.edgeInfo, None)
      } else photo
    } else photo
  }


  private def encryptAlbum(album: Album, friendIds: List[Long]): Album = {
    val coverphoto = album.coverPhoto map (encryptPhoto(_, friendIds))
    val encryptedPhotos = new ListBuffer[Photo]
    album.photos foreach ( (photo) => {
      encryptedPhotos += encryptPhoto(photo, friendIds)
    })
    Album(None,coverphoto,album.name,album.count,encryptedPhotos.toList, album.edgeInfo)
  }

  private def decryptAlbum(album: api.Album): api.Album ={
    val coverPhoto = album.coverPhoto map (decryptPhoto(_))
    val decryptedPhotos = new ListBuffer[Photo]
    album.photos foreach ( (photo) => {
      decryptedPhotos += decryptPhoto(photo)
    })
    Album(album.id,coverPhoto,album.name,album.count,decryptedPhotos.toList, album.edgeInfo)
  }


  private def sendAuthenticatedReq(req: HttpRequest) : Future[HttpResponse] ={
    val authChallengePipeline: HttpRequest => Future[AuthChallenge] = (sendReceive ~> unmarshal[AuthChallenge])
    for {
        authChallenge <-  authChallengePipeline(Get(authPrefix))
        reqExecFuture <- Future{
//          println("Received challenge: " + authChallenge)
          val signingContent = userId + ":" + authChallenge.challenge + ":" + req.uri.toString + ":" + req.entity.asString
          //        println("signing content: " + signingContent + "!!!!")
          val dsig = Crypto.sign(signingContent, privateKey)
          val authToken = AuthToken(userId, authChallenge.challenge, dsig)
          val pipeline: HttpRequest => Future[HttpResponse] = (addHeader("X-Auth", authToken.toJson.compactPrint)
            ~> sendReceive)
          pipeline(req)
        }
        resp <- reqExecFuture
      } yield resp
  }

  private def respConvertTo[T:JsonReader](futureResp:Future[HttpResponse]) : Future[T] = {
    for {
      response <- futureResp
      result <- Future {
          val jsonAst = JsonParser(response.entity.asString)
          jsonAst.convertTo[T]
      }
    } yield result
  }




  override def receive: Receive = {
    case CreatePost =>
      val message = faker.Lorem.sentence()
      val description = faker.Lorem.sentence()
      val link = "http://www." + faker.Internet.domain_name + "/" + Random.alphanumeric.take(10).mkString
      val post:api.Post = api.Post(None,message,None,Some(description), Some(link),None, None)
      if (!pageOrUserRoller) {
        val randomFriendIds = getRandomFriendIds
        val encPost = encryptPost(post, randomFriendIds)
        sendAuthenticatedReq(Put(restUrlBuilder(userPrefix, randomFriendIds(Random.nextInt(randomFriendIds.size)), Some(postsSuffix)), encPost)) foreach defaultRespHdlr
      } else sendAuthenticatedReq(Put(restUrlBuilder(pagePrefix, ClientConfig.pageIds.get(Random.nextInt(ClientConfig.pageIds.size())), Some(postsSuffix)), post)) foreach defaultRespHdlr



    case CreatePostWithPhotoAllFriendsShare=>
      val message = faker.Lorem.sentence()
      val description = faker.Lorem.sentence()
      val link = "http://www." + faker.Internet.domain_name + "/" + Random.alphanumeric.take(10).mkString
      val photolink = "http://www." + faker.Internet.domain_name + "/" + Random.alphanumeric.take(10).mkString + ".jpg"
      val data = new Array[Byte](100+Random.nextInt(500))
      Random.nextBytes(data)
      val photo = Photo(None,Some(photolink), Some(Crypto.encode(data)), None, None)
      val post = api.Post(None,message,Some(photo),Some(description), Some(link),None, None)
      if (!pageOrUserRoller) {
        val encPost = encryptPost(post, friendIds.toList)
        sendAuthenticatedReq(Put(restUrlBuilder(userPrefix, friendIds(Random.nextInt(friendIds.size)), Some(postsSuffix)), encPost)) foreach defaultRespHdlr
      } else sendAuthenticatedReq(Put(restUrlBuilder(pagePrefix, ClientConfig.pageIds.get(Random.nextInt(ClientConfig.pageIds.size())), Some(postsSuffix)), post)) foreach defaultRespHdlr


    case CreatePostWithPhoto =>
      val message = faker.Lorem.sentence()
      val description = faker.Lorem.sentence()
      val link = "http://www." + faker.Internet.domain_name + "/" + Random.alphanumeric.take(10).mkString
      val photolink = "http://www." + faker.Internet.domain_name + "/" + Random.alphanumeric.take(10).mkString + ".jpg"
      val data = new Array[Byte](100+Random.nextInt(500))
      Random.nextBytes(data)
      val photo = Photo(None,Some(photolink), Some(Crypto.encode(data)), None, None)
      val post = api.Post(None,message,Some(photo),Some(description), Some(link),None, None)

      if (!pageOrUserRoller) {
        val randomFriendIds = getRandomFriendIds
        val encPost = encryptPost(post, randomFriendIds)
        sendAuthenticatedReq(Put(restUrlBuilder(userPrefix, randomFriendIds(Random.nextInt(randomFriendIds.size)), Some(postsSuffix)), encPost)) foreach defaultRespHdlr
      } else sendAuthenticatedReq(Put(restUrlBuilder(pagePrefix, ClientConfig.pageIds.get(Random.nextInt(ClientConfig.pageIds.size())), Some(postsSuffix)), post)) foreach defaultRespHdlr


    case PostPhoto =>
      val photolink = "http://www." + faker.Internet.domain_name + "/" + Random.alphanumeric.take(10).mkString + ".jpg"
      val data = new Array[Byte](100+Random.nextInt(500))
      Random.nextBytes(data)
      val photo = Photo(None,Some(photolink), Some(Crypto.encode(data)), None, None)
      if(!pageOrUserRoller){
        val randomFriendIds = getRandomFriendIds
        val encPhoto = encryptPhoto(photo, randomFriendIds)
        sendAuthenticatedReq(Put(restUrlBuilder(userPrefix, userId, Some(photosSuffix)), encPhoto)) foreach defaultRespHdlr
      }
      else sendAuthenticatedReq(Put(restUrlBuilder(pagePrefix, ClientConfig.pageIds.get(Random.nextInt(ClientConfig.pageIds.size)), Some(photosSuffix)), photo)) foreach defaultRespHdlr

    case CreateAlbum =>
      val numPhotos = 5 + Random.nextInt(20)
      val photos = new ListBuffer[Photo]()
      for(i <-0 to numPhotos){
        val photolink = "http://www." + faker.Internet.domain_name + "/" + Random.alphanumeric.take(10).mkString + ".jpg"
        val data = new Array[Byte](100+Random.nextInt(500))
        Random.nextBytes(data)
        val photo = Photo(None,Some(photolink), Some(Crypto.encode(data)), None, None)
        photos += photo
      }
      val album = Album(None, None, Some(faker.Lorem.sentence()),None, photos.toList, None)
      if(!pageOrUserRoller) {
        val randomFriendIds = getRandomFriendIds
        val encAlbum = encryptAlbum(album, randomFriendIds)
        sendAuthenticatedReq(Put(restUrlBuilder(userPrefix, userId, Some(albumsSuffix)), encAlbum)) foreach defaultRespHdlr
      }
      else sendAuthenticatedReq(Put(restUrlBuilder(pagePrefix, ClientConfig.pageIds.get(Random.nextInt(ClientConfig.pageIds.size)), Some(albumsSuffix)), album)) foreach defaultRespHdlr

    case GetProfile =>
      if(!pageOrUserRoller)
        sendAuthenticatedReq(Get(userPrefix+"/"+friendIds(Random.nextInt(friendIds.size)))) foreach defaultRespHdlr
      else sendAuthenticatedReq(Get(pagePrefix+"/" + ClientConfig.pageIds.get(Random.nextInt(ClientConfig.pageIds.size)))) foreach defaultRespHdlr
    case GetPosts =>
      if(!pageOrUserRoller)
        sendAuthenticatedReq(Get(restUrlBuilder(userPrefix, friendIds(Random.nextInt(friendIds.size)), Some(postsSuffix)))) foreach defaultRespHdlr
      else sendAuthenticatedReq(Get(restUrlBuilder(pagePrefix, ClientConfig.pageIds.get(Random.nextInt(ClientConfig.pageIds.size)), Some(postsSuffix)))) foreach defaultRespHdlr
    case GetPhotos =>
      if(!pageOrUserRoller)
        sendAuthenticatedReq(Get(restUrlBuilder(userPrefix, friendIds(Random.nextInt(friendIds.size)), Some(photosSuffix)))) foreach defaultRespHdlr
      else sendAuthenticatedReq(Get(restUrlBuilder(pagePrefix, ClientConfig.pageIds.get(Random.nextInt(ClientConfig.pageIds.size)), Some(photosSuffix)))) foreach defaultRespHdlr
    case GetAlbums =>
      if(!pageOrUserRoller)
        sendAuthenticatedReq(Get(restUrlBuilder(userPrefix, friendIds(Random.nextInt(friendIds.size)), Some(albumsSuffix)))) foreach defaultRespHdlr
      else sendAuthenticatedReq(Get(restUrlBuilder(pagePrefix, ClientConfig.pageIds.get(Random.nextInt(ClientConfig.pageIds.size)), Some(albumsSuffix)))) foreach defaultRespHdlr

    case CreateAPage =>
      val title = faker.Lorem.sentence()
      val description = faker.Lorem.paragraph()
      val website = "http://www." + faker.Internet.domain_name
      val page = Page(None, Some(title), Some(description), Some(website),None)
      val futureResp = sendAuthenticatedReq(Put(pagePrefix, page))
      val futurePageResp = respConvertTo[Page](futureResp)

      val createdPage = Await.result[Page](futurePageResp, Duration.create(10, TimeUnit.SECONDS))
      ClientConfig.pageIds.add(createdPage.id.get)
      context.actorSelection(ActorNames.FakeUserCoOrdinatorRef) ! RequestComplete

      futurePageResp foreach((newPage)=>{
        ClientConfig.pageIds.add(newPage.id.get)

        context.actorSelection(ActorNames.FakeUserCoOrdinatorRef) ! RequestComplete
      })

    case BootStartFriendList(avgFriends) =>
      val pipeline: HttpRequest => Future[HttpResponse] = sendReceive
      pipeline(Get(urlPrefix+"/bootstrap/avgfriends/" + avgFriends)) foreach {(httpresp)=>
        context.actorSelection(ActorNames.FakeUserCoOrdinatorRef)  ! RequestComplete
      }

    case SignUp =>
      val first_name = faker.Name.first_name
      val last_name = faker.Name.last_name
      val name =  first_name + " " + last_name
      val randdate = (1970+ Random.nextInt(35)) + "-" + (Random.nextInt(12)+1).formatted("%02d") +"-" + (Random.nextInt(28)+1).formatted("%02d") + "T01:01:01"
      val birthday = DateTime.fromIsoDateTimeString(randdate)
      val gender = Random.nextInt(2) match {
        case 0 => "male"
        case 1 => "female"
      }

      // get user password and file input stream
      val keypass = "keypass".toCharArray
      val keyAlias = "pk"+(actorSuffix.toInt%1000)
      try{

        privateKey = ClientConfig.keystore.getKey(keyAlias, keypass).asInstanceOf[PrivateKey]
        publicKey = ClientConfig.keystore.getCertificate(keyAlias).getPublicKey
//        println(System.currentTimeMillis() - startTime)

        val userPipeline: HttpRequest => Future[User] = (sendReceive
            ~> unmarshal[User]
          )
        val user = api.User(None, first_name, last_name, Some(faker.Internet.free_email(name)), birthday, Some(gender), Crypto.encode(publicKey.getEncoded))

        userPipeline(Put(userPrefix, user)) map ((createdUser) => {
          userId = createdUser.id.get
          friendIds = List(userId).toIndexedSeq
          ClientConfig.userIdActorNumberMapping.put(userId, actorSuffix.toInt % 1000)
          context.actorSelection(ActorNames.FakeUserCoOrdinatorRef) ! RequestComplete
        })
      }catch{
        case ex:Throwable=> println("Failed to retrieve public key from Keystore: "+ex)

      }

    case PerformDemoFlow =>
      self ! CreatePostWithPhotoAllFriendsShare
      friendIds.slice(0,5) foreach ((friendId)=>{
        context.actorSelection(ActorNames.FakeUserPrefixRef+ClientConfig.userIdActorNumberMapping(friendId)) ! CreatePostWithPhotoAllFriendsShare
        Thread.sleep(200)
      })
      context.system.scheduler.scheduleOnce(Duration.create(5, TimeUnit.SECONDS), new Runnable(){
        override def run(): Unit = self ! DemoEncryption
      })
    case DemoEncryption =>
      var futureResp = sendAuthenticatedReq(Get(restUrlBuilder(userPrefix, userId, Some(postsSuffix))))
      val postResp = respConvertTo[api.Posts](futureResp)
      postResp foreach(posts =>{
        posts.data.slice(0,3) foreach (post=>{
          println("Encrypted Data: " + post.toJson.prettyPrint)
          println("Decrypted Data: "+decryptPost(post).toJson.prettyPrint)
        })
      })
//      futureResp = sendAuthenticatedReq(Get(restUrlBuilder(userPrefix, userId, Some(photosSuffix))))
//      val photosResp = respConvertTo[api.Photos](futureResp)
//      photosResp foreach(photos =>{
//        println(photos.toJson.prettyPrint)
//        photos.data.slice(0,3) foreach (photo=>{
//          println(decryptPhoto(photo).toJson.prettyPrint)
//        })
//      })
//      futureResp = sendAuthenticatedReq(Get(restUrlBuilder(userPrefix, userId, Some(albumsSuffix))))
//      val albumsResp = respConvertTo[api.Albums](futureResp)
//      albumsResp foreach(albums =>{
//        albums.data.slice(0,3) foreach (album=>{
//          println(album.toJson.prettyPrint)
//          println(decryptAlbum(album).toJson.prettyPrint)
//        })
//      })


    case AddFriend =>
      val randFriendId = Random.nextInt(ClientConfig.numUsers)+1
      val httpResponse = sendAuthenticatedReq(Put(restUrlBuilder(userPrefix, userId, Some(friendsSuffix)), ObjectId(randFriendId)))
      val result = Await.result(httpResponse, Duration.create(waitTime, TimeUnit.SECONDS))
      if(result.status.isFailure) {
        self ! AddFriend
      }
      else {
        context.actorSelection(ActorNames.FakeUserCoOrdinatorRef) ! RequestComplete
      }

    case InitUserFriends =>
      val futureResp = sendAuthenticatedReq(Get(restUrlBuilder(userPrefix, userId, Some(friendsSuffix))))
      val futureFLResp = respConvertTo[FriendList](futureResp)
        futureFLResp foreach((friendList)=>{
        val friendIds = new mutable.ListBuffer[Long]
        friendIds += userId
        friendList.data foreach ((friend) => {
          friendIds += friend.id.get
        })
        this.friendIds = friendIds.toIndexedSeq
        context.actorSelection(ActorNames.FakeUserCoOrdinatorRef) ! RequestComplete
      })

  }

}
