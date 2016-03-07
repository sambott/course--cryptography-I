package org.bott.test

import java.io.{RandomAccessFile, File, FileInputStream}

import scala.collection.immutable.Iterable

/**
  * This file is subject to the terms and conditions defined in
  * file 'LICENSE.txt', which is part of this source code package.
  *
  * Sam Bott, 06/03/2016.
  */

class BackwardFileStream(fileName: String, chunkSize: Int = 1024) {

  protected val file = new RandomAccessFile(fileName, "r")
  protected val fileLength = file.length
  protected val lastChunkSize: Int = {
    if (fileLength % chunkSize == 0) chunkSize
    else (fileLength % chunkSize).toInt
  }
  protected val numberOfChunks: Int = 1 + ((fileLength - lastChunkSize) / chunkSize).toInt

  protected def getChunk(i: Int): Seq[Byte] = {
    val bytesToTake = if (i == numberOfChunks - 1) lastChunkSize else chunkSize
    val buffer = new Array[Byte](bytesToTake)
    file.seek(i * chunkSize)
    file.read(buffer)
    buffer.toVector
  }

  def stream: Stream[Seq[Byte]] = Stream.from(numberOfChunks - 1, -1).takeWhile(-1 != _) map getChunk

}
