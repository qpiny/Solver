package org.rejna.util

import scala.collection.mutable.{ ArrayBuffer, ListBuffer }

class BitStreamWriter {
  private var stb = 0
  private var stb_len = 0
  private val content = ArrayBuffer[Byte]()

  def add(value: Int, len: Int): Unit = {
    val req_bits = 8 - stb_len
    if (len >= req_bits) {
      ((value << stb_len) | stb).asInstanceOf[Byte] +=: content
      //((stb | ((value >> (len - req_bits)) & ((1 << req_bits) - 1))).asInstanceOf[Byte]) +=: content
      stb = 0
      stb_len = 0
      if (len != req_bits)
        add(value >> req_bits, len - req_bits)
    } else {
      stb |= (value & ((1 << len) - 1)) << stb_len
      stb_len += len
    }
  }

  def toByteArray = {
    if (stb_len > 0)
      (content.clone += stb.asInstanceOf[Byte]).toArray
    else
      content.toArray
  }
}

class BitStreamReader(init: Byte*) {
  private var stb = 0
  private var stb_len = 0
  private val content = ListBuffer[Byte](init: _*)

  def get(len: Int): Int = {
    if (len <= stb_len) {
      val r = stb & ((1 << len) - 1)
      stb_len -= len
      stb >>= len
      r
    } else if (stb_len == 0) {
      var r = 0
      (0 until len - 7 by 8).foreach(i => {
        r |= (0xff & content.remove(content.length - 1)) << i
      })
      val v = len % 8
      if (v != 0) {
        stb = 0xff & content.remove(content.length - 1)
        stb_len = 8
        r <<= v
        r |= get(v)
      }
      r
    } else { // 0 < stb_len < len
      val l = len - stb_len
      val sl = stb_len
      var r = stb
      stb = 0
      stb_len = 0
      r |= get(l) << sl
      r
    }

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
