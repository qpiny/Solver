package org.rejna.solver.mancala

object Player extends Enumeration {
  type Player = Value
  val First, Second = Value
}

object Action extends Enumeration {
  type Action = Value
  val NextPlayer, PlayAgain, InvalidMove, FirstWin, SecondWin, TieGame = Value
}
