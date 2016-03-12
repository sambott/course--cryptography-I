import java.nio.{ByteOrder, ByteBuffer}
import java.security.SecureRandom
import javax.crypto.Cipher
import javax.crypto.spec.IvParameterSpec
import javax.crypto.spec.SecretKeySpec
import org.apache.commons.codec.binary.Hex

import scala.annotation.tailrec
import scala.language.postfixOps

object Encryptor {

  // NB This should not be considered a secure, robust AES implementation - use the javax.crypto.Cipher libraries

  implicit class PimpedByteArray(left: Array[Byte]) {
    def ^(right: Array[Byte]) = {
      require(left.length == right.length)
      left zip right map (p => (p._1 ^ p._2).asInstanceOf[Byte])
    }
  }

  def encryptCbc(keyHex: String, message: String): String = {
    val iv = newIv
    val key = Hex.decodeHex(keyHex.toCharArray)
    val messageBlocks = pad(message.getBytes) grouped 16 toList
    @tailrec
    def inner(in: List[Array[Byte]], out: List[Array[Byte]], v: Array[Byte]): List[Array[Byte]] = { in match {
      case Nil => out
      case h :: t =>
        val c = encryptBlock(key, h ^ v)
        inner(t, out :+ c, c)
    }}
    val cipherText = iv :: inner(messageBlocks, Nil, iv)
    Hex.encodeHexString(cipherText.flatten.toArray)
  }

  def decryptCbc(keyHex: String, cipherTextHex: String): String = {
    val (iv,cipherText) = Hex.decodeHex(cipherTextHex.toCharArray).splitAt(16)
    val key = Hex.decodeHex(keyHex.toCharArray)
    val cipherTextBlocks = cipherText grouped 16 toList
    @tailrec
    def inner(in: List[Array[Byte]], out: List[Array[Byte]], chained: Array[Byte]): List[Array[Byte]] = { in match {
      case Nil => out
      case h :: t =>
        val m = decryptBlock(key, h)
        inner(t, out :+ (m ^ chained), h)
    }}
    val message = inner(cipherTextBlocks, Nil, iv)
    new String(unpad(message.flatten.toArray))
  }

  def encryptCtr(keyHex: String, message: String): String = {
    val nonce = newIv
    val messageBlocks = message.getBytes grouped 16 toList
    val blockStream = ctrStream(keyHex, nonce)
    val cipherText = messageBlocks zip blockStream map {
      case (m,s) =>
        val s1 = s take m.length
        s1 ^ m
    }
    val outputBlocks = nonce :: cipherText
    Hex.encodeHexString(outputBlocks.flatten.toArray)
  }

  def decryptCtr(keyHex: String, cipherTextHex: String): String = {
    val (nonce,cipherText) = Hex.decodeHex(cipherTextHex.toCharArray).splitAt(16)
    val key = Hex.decodeHex(keyHex.toCharArray)
    val cipherTextBlocks = cipherText grouped 16 toList
    val blockStream = ctrStream(keyHex, nonce)
    val message = cipherTextBlocks zip blockStream map {
      case (c,s) =>
        val s1 = s take c.length
        s1 ^ c
    }
    new String(message.flatten.toArray)
  }

  private def aesSingleBlock(key: Array[Byte], message: Array[Byte], mode: Int): Array[Byte] = {
    require(message.length == 16)
    require(key.length == 16)
    val skeySpec: SecretKeySpec = new SecretKeySpec(key, "AES")
    val cipher: Cipher = Cipher.getInstance("AES/ECB/NoPadding")
    cipher.init(mode, skeySpec)
    val output = cipher.doFinal(message)
    output
  }

  private def encryptBlock(key: Array[Byte], message: Array[Byte]) = aesSingleBlock(key, message, Cipher.ENCRYPT_MODE)

  private def decryptBlock(key: Array[Byte], message: Array[Byte]) = aesSingleBlock(key, message, Cipher.DECRYPT_MODE)

  def ctrStream(keyHex: String, nonce: Array[Byte]) = {
    val key = Hex.decodeHex(keyHex.toCharArray)
    val startV = new Array[Byte](16)
    nonce.copyToArray(startV)
    def inner(ctr: Long): Stream[Array[Byte]] = {
      encryptBlock(key, createCtrMessage(nonce, ctr)) #:: inner(ctr + 1)
    }
    inner(0)
  }

  def createCtrMessage(nonce: Array[Byte], ctr: Long) = {
    val (first,nonceLast) = nonce splitAt 8
    val lastBuffer = ByteBuffer.allocate(8).order(ByteOrder.BIG_ENDIAN).put(nonceLast)
    lastBuffer.rewind()
    val lastLong = lastBuffer.getLong + ctr
    lastBuffer.rewind()
    lastBuffer.putLong(lastLong)
    first ++ lastBuffer.array()
  }

  private def pad(in: Array[Byte]): Array[Byte] = {
    if (in.length % 16 == 0) in
    else {
      val padLength: Int = 16 - (in.length % 16)
      val paddingBytes = Iterator.continually(padLength.asInstanceOf[Byte]).take(padLength).toArray
      in ++ paddingBytes
    }
  }

  private def unpad(in: Array[Byte]): Array[Byte] = {
    val unpaddedLength: Int = in.length - in.last
    val (message,pad) = in splitAt unpaddedLength
    assert(pad forall ( in.last == _ ))
    message
  }

  def newIv = {
    val iv = new Array[Byte](16)
    new SecureRandom().nextBytes(iv)
    iv
  }







  // To TEST MY IMPLEMENTATIONS =>

  def javasEncryptCbc(keyHex: String, initVectorHex: String, value: String): String = {
    val iv: IvParameterSpec = new IvParameterSpec(Hex.decodeHex(initVectorHex.toCharArray))
    val skeySpec: SecretKeySpec = new SecretKeySpec(Hex.decodeHex(keyHex.toCharArray), "AES")
    val cipher: Cipher = Cipher.getInstance("AES/CBC/PKCS5PADDING")
    cipher.init(Cipher.ENCRYPT_MODE, skeySpec, iv)
    val encrypted: Array[Byte] = cipher.doFinal(value.getBytes)
    val hex = new String(Hex.encodeHex(encrypted, true))
    hex
  }

  def javasDecryptCbc(keyHex: String, encryptedHex: String): String = {
    val (initVector,encrypted) = Hex.decodeHex(encryptedHex.toCharArray).splitAt(16)
    val iv: IvParameterSpec = new IvParameterSpec(initVector)
    val skeySpec: SecretKeySpec = new SecretKeySpec(Hex.decodeHex(keyHex.toCharArray), "AES")
    val cipher: Cipher = Cipher.getInstance("AES/CBC/PKCS5PADDING")
    cipher.init(Cipher.DECRYPT_MODE, skeySpec, iv)
    val original: Array[Byte] = cipher.doFinal(encrypted)
    new String(original)
  }

  def javasEncryptCtr(keyHex: String, initVectorHex: String, value: String): String = {
    val iv: IvParameterSpec = new IvParameterSpec(Hex.decodeHex(initVectorHex.toCharArray))
    val skeySpec: SecretKeySpec = new SecretKeySpec(Hex.decodeHex(keyHex.toCharArray), "AES")
    val cipher: Cipher = Cipher.getInstance("AES/CTR/NOPADDING")
    cipher.init(Cipher.ENCRYPT_MODE, skeySpec, iv)
    val encrypted: Array[Byte] = cipher.doFinal(value.getBytes)
    val hex = new String(Hex.encodeHex(encrypted, true))
    hex
  }

  def javasDecryptCtr(keyHex: String, encryptedHex: String): String = {
    val (initVector,encrypted) = Hex.decodeHex(encryptedHex.toCharArray).splitAt(16)
    val iv: IvParameterSpec = new IvParameterSpec(initVector)
    val skeySpec: SecretKeySpec = new SecretKeySpec(Hex.decodeHex(keyHex.toCharArray), "AES")
    val cipher: Cipher = Cipher.getInstance("AES/CTR/NOPADDING")
    cipher.init(Cipher.DECRYPT_MODE, skeySpec, iv)
    val original: Array[Byte] = cipher.doFinal(encrypted)
    new String(original)
  }
}
