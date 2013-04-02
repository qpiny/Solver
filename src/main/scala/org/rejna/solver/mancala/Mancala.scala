package org.rejna.solver.mancala

import akka.actor.ActorRefFactory
import com.typesafe.config.Config
import org.rejna.solver._
import org.rejna.util.BitStream

object Player extends Enumeration {
  type Player = Value
  val First, Second = Value
}

object Action extends Enumeration {
  type Action = Value
  val NextPlayer, PlayAgain, InvalidMove, FirstWin, SecondWin, TieGame = Value
}
