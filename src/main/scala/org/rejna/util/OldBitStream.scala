package org.rejna.util

import collection.mutable.{ ArrayStack, ArrayBuffer }
import sbinary.Input
import sbinary.Output

@deprecated("Use instead BitStreamReader and BitStreamWriter", "now")
class BitStream(init: Byte*) {
  private var stb: Int = 0
  private var stb_len = 0
  private val content = new ArrayStack[Byte]
  content ++= init.reverse

  def push(value: Int, len: Int): Unit = {
    val req_bits = 8 - stb_len
    if (len >= req_bits) {
      content.push((stb | ((value >> (len - req_bits)) & ((1 << req_bits) - 1))).asInstanceOf[Byte])
      stb = 0
      stb_len = 0
      if (len != req_bits)
        push(value, len - req_bits)
    } else {
      stb_len += len
      stb |= (value & ((1 << len) - 1)) << (8 - stb_len)
    }
  }

  def pop(len: Int): Int = {
    if (len <= stb_len) {
      val r = (stb >> (8 - stb_len)) & ((1 << len) - 1)
      stb &= ~((1 << (len + 8 - stb_len)) - 1)
      stb_len -= len
      r
    } else if (stb_len == 0) {
      var r = 0
      (0 until (len / 8)).foreach(i => r |= (0xff & content.pop) << (i * 8))
      val v = len % 8
      if (v != 0) {
        stb = 0xff & content.pop
        stb_len = 8
        r = (pop(v) << (8 * (len / 8))) | r
        r
      } else
        r
    } else {
      val s = stb
      val l = stb_len
      stb = 0
      stb_len = 0
      val r = (pop(len - l) << l) | (s >> (8 - l))
      r
    }

  }

  def pad = {
    if (stb_len > 0) {
      content.push(stb.asInstanceOf[Byte])
      stb = 0
      stb_len = 0
    }
  }

  def toArray = {
    pad
    content.toArray
  }
}