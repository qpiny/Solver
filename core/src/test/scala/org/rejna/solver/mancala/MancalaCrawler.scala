package org.rejna.solver.mancala

import Action._

object MancalaCrawler extends App {
  def play(game: Game): Unit = {
    println(game)
    for (c <- game.children.zipWithIndex;
        if c._1.isDefined)
      print(s"[${c._2}] ")
    println
    println("Choice :")
    val (g, a) = game.play(Console.readInt)
    a match {
      case NextPlayer | PlayAgain =>
        play(g)
      case InvalidMove =>
        println("Invalid choice")
        play(game)
      case FirstWin =>
        println("First player wins")
      case SecondWin =>
        println("Second player wins")
      case TieGame =>
        println("Tie game")
    }
  }

  override def main(args: Array[String]) = {
//    val startGame = new Game(
//      Array(0, 2, 1, 10, 1, 0, 15),
//      Array(0, 0, 9, 0, 7, 0, 3), SecondPlayer)

    val startGame = new Game(
    Array(6, 1, 4, 0, 0, 0, 13),
    Array(0, 0, 0, 0, 8, 8, 8), FirstPlayer)
    play(startGame)
  }
}