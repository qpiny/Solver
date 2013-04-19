package org.rejna.util

import org.scalacheck._
import org.scalacheck.Test.Parameters

object BitStreamCheck extends Properties("BitStream") {
  case class Element(size: Int, value: Int)

  val genEl = for {
    size <- Gen.choose(1, 30)
    data <- Gen.choose(0, (1 << size) - 1)
  } yield Element(size, data)

  val genStack = Gen.sized(size =>
    Gen.listOfN(size, genEl))

  val p = Prop.forAll(genStack)(l => {
    val writer = new BitStreamWriter
    for (e <- l)
      writer.add(e.value, e.size)
    val reader = new BitStreamReader(writer.toByteArray: _*)
    l.foldLeft(true)((r, el) => r && reader.get(el.size) == el.value)
  })

  p.check(new Parameters.Default {
    override val minSuccessfulTests = 1000
  })
}