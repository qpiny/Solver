package org.rejna.util

import scala.collection.mutable.ListBuffer
object StringColumn {

  def makeColumn(text: String*): String = {
    val columns = text.map(t => t.split("\\r?\\n"))
    val columnSize = columns.map(c => c(0).length())
    val output = new StringBuilder
    var index = 0
    var continue = true
    while (continue) {
      continue = false
      for (c <- columns.zipWithIndex) {
        output.append(("#%-" + columnSize(c._2) + "s").format(if (index >= c._1.length) "" else {
          continue = true
          c._1(index)
        }))
      }
      index += 1
      output.append("#\n")
    }
    output.toString
  }
}
