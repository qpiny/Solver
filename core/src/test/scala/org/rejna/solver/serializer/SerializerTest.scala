package org.rejna.solver.serializer

import org.scalacheck._
import org.scalacheck.Test.Parameters
import sbinary.Operations

object SerializerCheck extends Properties("Serializer") {
  import org.rejna.solver.mancala._

  val gameSerializerProp = Prop.forAll(MancalaCheck.genGame)(g1 => {
    val format = Game.GameFormat
    val data = Operations.toByteArray(g1)(format)
    val g2 = Operations.fromByteArray(data)(format)
    g1 == g2
  })
  
  val GameStatSerializeProp = Prop.forAll(MancalaCheck.genGameStat)(gs1 => {
    val format = GameStat.GameStatFormat
    val data = Operations.toByteArray(gs1)(format)
    val gs2 = Operations.fromByteArray(data)(format)
    gs1 == gs2
  })
  
  gameSerializerProp.check(new Parameters.Default {
    override val minSuccessfulTests = 1000
  })
  
  GameStatSerializeProp.check(new Parameters.Default {
    override val minSuccessfulTests = 1000
  })
}