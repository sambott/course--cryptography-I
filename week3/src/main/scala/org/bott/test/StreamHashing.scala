package org.bott.test

import akka.stream.scaladsl.Source
import akka.util.ByteString

import scala.annotation.tailrec
import scala.collection.immutable

/**
  * This file is subject to the terms and conditions defined in
  * file 'LICENSE.txt', which is part of this source code package.
  *
  * Sam Bott, 06/03/2016.
  */

object StreamHashing {

  protected def sha256(bytes: Seq[Byte]): Array[Byte] = {
    val sha = java.security.MessageDigest.getInstance("SHA-256")
    sha digest bytes.toArray
  }

  protected def backwardFileBlocks(file: String): Stream[Seq[Byte]] = {
    new BackwardFileStream(file).stream
  }

  protected def streamHasher(backwardBlockStream: Stream[Seq[Byte]]) = {
    @tailrec
    def inner(bs: Stream[Seq[Byte]], out: Stream[Seq[Byte]], lastHash: Seq[Byte]): Stream[Seq[Byte]] = {
      bs match {
        case Stream.Empty => lastHash #:: out
        case h #:: t =>
          val nextBlock = h ++ lastHash
          inner(t, nextBlock #:: out, sha256(nextBlock))
      }
    }
    inner(backwardBlockStream, Stream.Empty, Nil)
  }

  def createHashedFileStream(filename: String) = {
    val bf = backwardFileBlocks(filename)
    streamHasher(bf)
  }

}
