import javax.crypto.Cipher
import javax.crypto.spec.IvParameterSpec
import javax.crypto.spec.SecretKeySpec
import org.apache.commons.codec.binary.Hex

object Encryptor {

  def encryptCbc(keyHex: String, initVectorHex: String, value: String): String = {
    val iv: IvParameterSpec = new IvParameterSpec(Hex.decodeHex(initVectorHex.toCharArray))
    val skeySpec: SecretKeySpec = new SecretKeySpec(Hex.decodeHex(keyHex.toCharArray), "AES")
    val cipher: Cipher = Cipher.getInstance("AES/CBC/PKCS5PADDING")
    cipher.init(Cipher.ENCRYPT_MODE, skeySpec, iv)
    val encrypted: Array[Byte] = cipher.doFinal(value.getBytes)
    val hex = new String(Hex.encodeHex(encrypted, true))
    hex
  }

  def decryptCbc(keyHex: String, encryptedHex: String): String = {
    val (initVector,encrypted) = Hex.decodeHex(encryptedHex.toCharArray).splitAt(16)
    val iv: IvParameterSpec = new IvParameterSpec(initVector)
    val skeySpec: SecretKeySpec = new SecretKeySpec(Hex.decodeHex(keyHex.toCharArray), "AES")
    val cipher: Cipher = Cipher.getInstance("AES/CBC/PKCS5PADDING")
    cipher.init(Cipher.DECRYPT_MODE, skeySpec, iv)
    val original: Array[Byte] = cipher.doFinal(encrypted)
    new String(original)
  }

  def encryptCtr(keyHex: String, initVectorHex: String, value: String): String = {
    val iv: IvParameterSpec = new IvParameterSpec(Hex.decodeHex(initVectorHex.toCharArray))
    val skeySpec: SecretKeySpec = new SecretKeySpec(Hex.decodeHex(keyHex.toCharArray), "AES")
    val cipher: Cipher = Cipher.getInstance("AES/CTR/NOPADDING")
    cipher.init(Cipher.ENCRYPT_MODE, skeySpec, iv)
    val encrypted: Array[Byte] = cipher.doFinal(value.getBytes)
    val hex = new String(Hex.encodeHex(encrypted, true))
    hex
  }

  def decryptCtr(keyHex: String, encryptedHex: String): String = {
    val (initVector,encrypted) = Hex.decodeHex(encryptedHex.toCharArray).splitAt(16)
    val iv: IvParameterSpec = new IvParameterSpec(initVector)
    val skeySpec: SecretKeySpec = new SecretKeySpec(Hex.decodeHex(keyHex.toCharArray), "AES")
    val cipher: Cipher = Cipher.getInstance("AES/CTR/NOPADDING")
    cipher.init(Cipher.DECRYPT_MODE, skeySpec, iv)
    val original: Array[Byte] = cipher.doFinal(encrypted)
    new String(original)
  }
}
