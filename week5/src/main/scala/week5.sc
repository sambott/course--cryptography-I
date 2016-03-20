import org.apache.commons.codec.binary.Hex

import scala.annotation.tailrec

def intPow(a: Int, b: Int) = {
  require(a >= 0)
  require(b > 0)
  @tailrec
  def inner(acc: Int, n: Int): Int = {
    if (n == 0) acc else inner(acc * a, n - 1)
  }
  inner(1, b)
}

def dlog(maxXbase: Int, maxXexp: Int)(g: BigInt, p: BigInt, h: BigInt) = {
  val b = intPow(maxXbase, (maxXexp / 2.0).ceil.toInt) // x is less than b^2
  // => x = x0 * b + x1
  // => h = g^x = g^(x0 * b + x1) = (g^b)^x0 * g^x1 , x0,x1 in [0, b-1]
  // => h/(g^x1) = (b^b)^x0
  val gB = g.modPow(b, p)
  def left(x1: Int) = (g.modPow(x1, p).modInverse(p) * h) mod p
  def right(x0: Int) = gB.modPow(x0, p)
  val allRights = (0 until b).par.map(i => (right(i), i)).toMap
  val validXcomponents = for {
    thisX1 <- Stream.range(0, b).par
    nextLeft = left(thisX1)
    if allRights.keySet contains nextLeft
  } yield (allRights(nextLeft), thisX1)
  val (x0,x1) = validXcomponents.head
  x0.toLong * b + x1
}
def p = BigInt("13407807929942597099574024998205846127479365820592393377723561443721764030073546976801874298166903427690031858186486050853753882811946569946433649006084171")
def g = BigInt("11717829880366207009516117596335367088558084999998952205599979459063929499736583746670572176471460312928594829675428279466566527115212748467589894601965568")
def h = BigInt("3239475104050450443565264378728065788649097520952449527834792452971981976143292558073856937958553180532878928001494706097394108577585732452307673444020333")

val ans = dlog(2, 40)(g, p, h)
if (g.modPow(ans, p) == h) {
  println(ans)
}
