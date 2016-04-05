package dos.project4.common

object ActorNames {
  // create and start our service actor
  val FBSimulatorService = "facebook-simulator-service"
  val FBSimulatorServiceRef = "/user/facebook-simulator-service"
  val UserApiService = "user-api-service"
  val UserApiServiceRef = "/user/user-api-service"
  val PageApiService = "page-api-service"
  val PageApiServiceRef = "/user/page-api-service"
  val PhotoApiService = "photo-api-service"
  val PhotoApiServiceRef = "/user/photo-api-service"
  val PostApiService = "post-api-service"
  val PostApiServiceRef = "/user/post-api-service"
  val AlbumApiService = "album-api-service"
  val AlbumApiServiceRef = "/user/album-api-service"
  val AuthService = "auth-service"
  val AuthServiceRef = "/user/auth-service"

  val UserApiServiceManager = "user-api-service-manager"
  val UserApiServiceManagerRef = "/user/user-api-service-manager"
  val PageApiServiceManager = "page-api-service-manager"
  val PageApiServiceManagerRef = "/user/page-api-service-manager"
  val PhotoApiServiceManager = "photo-api-service-manager"
  val PhotoApiServiceManagerRef = "/user/photo-api-service-manager"
  val PostApiServiceManager = "post-api-service-manager"
  val PostApiServiceManagerRef = "/user/post-api-service-manager"
  val AlbumApiServiceManager = "album-api-service-manager"
  val AlbumApiServiceManagerRef = "/user/album-api-service-manager"
  val AuthApiServiceManager = "auth-service-manager"
  val AuthApiServiceManagerRef = "/user/auth-service-manager"

  val HttpConnectionManager = "http-connection-manager"
  val HttpConnectionManagerRef = "/user/http-connection-manager"
  val DataStoreService = "data-store-service"
  val DataStoreServiceRef = "/user/data-store-service"


  val FakeUserPrefix = "fb-fake-user:"
  val FakeUserPrefixRef = "/user/fb-fake-user:"
  val FakeUserCoOrdinator = "fb-fake-user-co"
  val FakeUserCoOrdinatorRef = "/user/fb-fake-user-co"
  val CreateOpTickManager = "create_op_tick"
  val GetOpTickManager = "get_op_tick"

}
