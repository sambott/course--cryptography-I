import org.apache.commons.codec.binary.Hex
import scala.annotation.tailrec
import scala.language.postfixOps
import scalaj.http.Http

import PaddingOracleAttack._

/**
  * This file is subject to the terms and conditions defined in
  * file 'LICENSE.txt', which is part of this source code package.
  *
  * Sam Bott, 13/03/2016.
  */

object Main extends App {

  /*
  Padding Oracle Attack on session:
  http://crypto-class.appspot.com/po?er=f20bdba6ff29eed7b046d1df9fb7000058b1ffb4210a580f748b4ac714c001bd4a61044426fb515dad3f21f18aa577c0bdf302936266926ff37dbf7035d5eeb4
   */

  val result = (0 until targetBlocks.length - 1).par flatMap (attackBlock(_, printProgress = true))
  var message = unpad(result.toArray)

  println(Hex.encodeHexString(result.toArray))
  println(new String(message))
}

object PaddingOracleAttack {


  val targetHex = "f20bdba6ff29eed7b046d1df9fb7000058b1ffb4210a580f748b4ac714c001bd4a61044426fb515dad3f21f18aa577c0bdf302936266926ff37dbf7035d5eeb4"
  val target = Hex decodeHex targetHex.toCharArray
  val targetBlocks = (target grouped 16).toList


  sealed trait PoResponse
  final case class Successful() extends PoResponse
  final case class PaddingIncorrect() extends PoResponse
  final case class PaddingCorrect() extends PoResponse

  val responseCodeMap = Map[Int,PoResponse](
    200 -> Successful(),
    403 -> PaddingIncorrect(),
    404 -> PaddingCorrect()
  )

  def tryCipher(bs: Array[Byte]): Int = {
    val erStr: String = Hex.encodeHexString(bs)
    val response = Http("http://crypto-class.appspot.com/po").
      param("er", erStr).asBytes
    response.code
  }

  def getPad(i: Int) = {
    Iterator continually i.asInstanceOf[Byte] take i toArray
  }

  def unpad(in: Array[Byte]): Array[Byte] = {
    val unpaddedLength: Int = in.length - in.last
    val (message,pad) = in splitAt unpaddedLength
    assert(pad forall ( in.last == _ ))
    message
  }

  def constructAttackCipherText(block: Int, p: Int, guess: List[Byte]): Array[Byte] = {
    require(16 > p)
    require(block < targetBlocks.length)
    val padSize = 16 - p
    require(guess.length == padSize)
    val (initial,rest) = targetBlocks splitAt block
    val (x,y) = rest.head splitAt p
    val z = rest.tail.head
    val mod = guess ^ getPad(padSize) ^ y
    val attackBlocks = initial :+ x :+ mod.toArray :+ z
    attackBlocks.flatten.toArray
  }

  def attackBlockAtPositionWithGuess(block: Int, position: Int, guess: List[Byte]): PoResponse = {
    val attackCipherText = constructAttackCipherText(block, position, guess)
    val responseCode = tryCipher(attackCipherText)
    responseCodeMap(responseCode)
  }

  def attackBlockAtPosition(block: Int, position: Int, knownEnding: List[Byte]) = {
    require(15 - position == knownEnding.length)
    var sucPos = -1
    val next = (0 to 255) indexWhere { g =>
      //Thread.sleep(100) // let's play nice
      attackBlockAtPositionWithGuess(block, position, g.asInstanceOf[Byte] :: knownEnding) match {
        case _: Successful =>
          sucPos = g
          false
        case _: PaddingCorrect =>
          true
        case _ =>
          false
      }
    }
    if (next != -1) next.asInstanceOf[Byte]
    else if (next == -1 && sucPos == -1) throw new RuntimeException("Valid byte not found")
    else sucPos.asInstanceOf[Byte]
  }

  def attackBlock(i: Int, printProgress: Boolean = false): Array[Byte] = {
    @tailrec
    def inner(p: Int, knownEnding: List[Byte]): List[Byte] = { p match {
      case -1 => knownEnding
      case _ =>
        val soFar = attackBlockAtPosition(i, p, knownEnding) :: knownEnding
        if (printProgress) println(s"Block: $i, sofar: ${Hex.encodeHexString(soFar.toArray)}")
        inner(p - 1, soFar)
    }}
    val g = inner(15, Nil)
    g.toArray
  }

  implicit class PimpedByteArray(left: Seq[Byte]) {
    def ^(right: Seq[Byte]) = {
      require(left.length == right.length)
      left zip right map (p => (p._1 ^ p._2).asInstanceOf[Byte])
    }
  }

}
