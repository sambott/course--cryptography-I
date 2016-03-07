import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import com.typesafe.config.ConfigFactory
import org.bott.test.StreamHashing

import org.apache.commons.codec.binary.Hex


val targetFile = "C:\\code\\courses\\crypto1\\week3\\data/6 - 1 - Introduction (11 min).mp4"
val testFile = "C:\\code\\courses\\crypto1\\week3\\data/6 - 2 - Generic birthday attack (16 min).mp4"
val testFileH0 = "03c08f4ee0b576fe319338139c045c89c3e8e9409633bea29442e21425006ea8"

def getH0(file: String) = {
  val hashedStream = StreamHashing.createHashedFileStream(file)
  val h0 = hashedStream.head
  Hex.encodeHexString(h0.toArray)
}

val testH0 = getH0(testFile)
testH0 == testFileH0
// TRUE - whoop


val answer = getH0(targetFile)
