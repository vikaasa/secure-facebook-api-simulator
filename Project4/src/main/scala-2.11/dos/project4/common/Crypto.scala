package dos.project4.common

import java.security._
import java.security.spec.X509EncodedKeySpec
import java.util.Base64
import javax.crypto.spec.{IvParameterSpec, SecretKeySpec}
import javax.crypto.{Cipher, KeyGenerator, SecretKey}

//object Crypto extends App{
object Crypto{
  val RSA_ALGO = "RSA/ECB/PKCS1Padding"
  val AES_ALGO = "AES/CBC/PKCS5Padding"
  val SECURE_RAND_GEN_ALGO = "SHA1PRNG"
  val SIGN_ALGO = "SHA256withRSA"
  val AES_KEY_SIZE = 256
  val RSA_KEY_SIZE = 1024

  def toAESKey(encodedKey:String):SecretKey= new SecretKeySpec(Crypto.decode(encodedKey),"AES")
  def toRSAPublicKey(encodedKey:String):PublicKey = {
    val keySpec = new X509EncodedKeySpec(Crypto.decode(encodedKey))
    val keyFactory = KeyFactory.getInstance("RSA")
    val pubKey = keyFactory.generatePublic(keySpec)
    pubKey
  }

  def encode(bytes:Array[Byte]) = Base64.getEncoder.encodeToString(bytes)
  def decode(encodedString: String): Array[Byte] = Base64.getDecoder.decode(encodedString)

  def decryptSharedKey(encryptedKey:String, privateKey:PrivateKey):SecretKey = decryptSharedKey(decode(encryptedKey), privateKey)
  def decryptSharedKey(encryptedKey:Array[Byte], privateKey:PrivateKey):SecretKey = {
    val cipher = Cipher.getInstance(RSA_ALGO)
    cipher.init(Cipher.DECRYPT_MODE, privateKey)
    val decBytes = cipher.doFinal(encryptedKey)
    new SecretKeySpec(decBytes,"AES")
  }

  def generateAESKey():SecretKey ={
    val generator = KeyGenerator.getInstance("AES")
    generator.init(AES_KEY_SIZE)
    generator.generateKey
  }

  def generateRSAKeyPair():KeyPair ={
    val generator = KeyPairGenerator.getInstance("RSA")
    generator.initialize(RSA_KEY_SIZE)
    generator.genKeyPair()
  }

  def encrypt(data:String, secretKey:SecretKey, initVector:Array[Byte]):String = {
    val cipher = Cipher.getInstance(AES_ALGO)
    cipher.init(Cipher.ENCRYPT_MODE, secretKey, new IvParameterSpec(initVector))
    encode(cipher.doFinal(data.getBytes("UTF-8")))
  }

  def encryptSharedKey(secretKey:SecretKey, publicKey: PublicKey):String = {
    val cipher = Cipher.getInstance(RSA_ALGO)
    cipher.init(Cipher.ENCRYPT_MODE, publicKey)
    encode(cipher.doFinal(secretKey.getEncoded))
  }

  def decrypt(data:String, secretKey:SecretKey, initVector:Array[Byte]):String = {
    val cipher = Cipher.getInstance(AES_ALGO)
    cipher.init(Cipher.DECRYPT_MODE, secretKey, new IvParameterSpec(initVector))
    new String(cipher.doFinal(decode(data)))
  }

  def generateInitVector():Array[Byte] ={
    val sRNG = SecureRandom.getInstance("SHA1PRNG")
    sRNG.setSeed(sRNG.generateSeed(32))
    val initV = new Array[Byte](16) // Block size of AES128,192,256 in CBC mode is 128bits or 16 bytes
    sRNG.nextBytes(initV)
    initV
  }

  def generateSRNG():Long = {
    val sRNG = SecureRandom.getInstance("SHA1PRNG")
    sRNG.setSeed(sRNG.generateSeed(32))
    sRNG.nextLong()
  }

  def sign(data:String, privateKey: PrivateKey):String = {
    val singer =  Signature.getInstance(SIGN_ALGO)
    singer.initSign(privateKey)
    singer.update(data.getBytes("UTF-8"))
    encode(singer.sign())
  }

  def verify(data:String, signedValue:String, pubKey: PublicKey):Boolean = {
    val verifier =  Signature.getInstance(SIGN_ALGO)
    verifier.initVerify(pubKey)
    verifier.update(data.getBytes("UTF-8"))
    verifier.verify(decode(signedValue))
  }


}
