package org.rejna.util

import scala.collection.mutable.{ ArrayBuffer, ListBuffer, Queue }

class BitStreamWriter {
  private var stb = 0L
  private var stbLen = 0
  private val content = ArrayBuffer[Byte]()

  def add(value: Long, len: Int): Unit = {
    if (len > 32)
      sys.error("Invalid parameter") // FIXME throw better exception
    stb |= value << stbLen
    stbLen += len
    while (stbLen >= 8) {
      content += (stb & 0xff).asInstanceOf[Byte] 
      stb >>= 8
      stbLen -= 8
    }
  }

  def toByteArray = {
    if (stbLen > 0)
      (content :+ (stb & 0xff).asInstanceOf[Byte]).toArray
    else
      content.toArray
  }
}

class BitStreamReader(val content: Queue[Byte]) {
  private var stb = 0L
  private var stbLen = 0
  
  def this(init: Byte*) = this(Queue[Byte](init: _*))

  def get(len: Int): Int = {
    if (len > 32)
      sys.error("Invalid parameter") // FIXME throw better exception
    while (stbLen < len) {
      stb |= (content.dequeue.asInstanceOf[Long] & 0xff) << stbLen
      stbLen += 8
    }
    val r = stb & (1 << len) -1
    stb >>= len
    stbLen -= len
    r.asInstanceOf[Int]
  }
}
