package org.rejna.solver.mancala

object Player {
  @inline final def foreach[U](f: Player => U) {
    f(FirstPlayer)
    f(SecondPlayer)
  }
}
abstract sealed class Player {
  val other: Player
  val id: Int
}

case object FirstPlayer extends Player {
  val other: Player = SecondPlayer
  val id = 0
}

case object SecondPlayer extends Player {
  val other: Player = FirstPlayer
  val id = 1
}

//object Player extends Enumeration {
//  type Player = Value
//  val First, Second = Value
//}

object Action extends Enumeration {
  type Action = Value
  val NextPlayer, PlayAgain, InvalidMove, FirstWin, SecondWin, TieGame = Value
}
