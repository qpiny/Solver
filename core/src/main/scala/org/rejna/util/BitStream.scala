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
//     ListBuffer → Builder → BufferLike → Growable
/*
 * 11 1111 1-010
 */
object Test {
  def doIt = {
    val bs = new BitStreamWriter
    bs.add(10, 3)
    bs.add(0xffff, 7)
    bs.add(0x111, 6)
    val b = bs.toByteArray
    println("%s should be [%d, %d]".format(b.mkString("(", ",", ")"), 0x47, 0xFA)) //    0100 01-11 1111 1-010
    val r = new BitStreamReader(b: _*)
    println("%d should be %d".format(r.get(3), 10 & ((1 << 3) -1)))
    println("%d should be %d".format(r.get(7), 0xffff & ((1 << 7) -1)))
    println("%d should be %d".format(r.get(6), 0x111 & ((1 << 6) -1)))
  }
}
