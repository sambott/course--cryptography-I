import org.apache.commons.codec.binary.Hex

// Workings for some cipher text manipulation

val iv = Hex.decodeHex("20814804c1767293b99f1d9cab3bc3e7".toCharArray)
val m = Hex.decodeHex("ac1e37bfb15599e5f40eef805488281d".toCharArray)


val orig = "Pay Bob 100$".getBytes
val repl = "Pay Bob 500$".getBytes

val (ivFirst,ivLAst) = iv splitAt orig.length


val c = (orig ^ repl ^ ivFirst) ++ ivLAst ++ m
Hex.encodeHexString(c)

implicit class PimpedByteArray(left: Array[Byte]) {
  def ^(right: Array[Byte]) = {
    require(left.length == right.length)
    left zip right map (p => (p._1 ^ p._2).asInstanceOf[Byte])
  }
}
