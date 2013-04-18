package org.rejna.solver.mancala

import org.scalatest._

class MancalaTest extends FlatSpec {
  "Game board" should "have correctly connected bean for first player" in {
    val startGame = new Game(
      Array(14, 1, 2, 3, 4, 5, 6),
      Array(7, 8, 9, 10, 11, 12, 13), Player.First)
    val expectedGame = new Game(
      Array(1, 3, 3, 4, 5, 6, 7),
      Array(8, 9, 10, 11, 12, 13, 13), Player.Second)
    expectResult(expectedGame) {
      startGame.play(0)._1
    }
  }

  it should "have correctly connected bean for second player" in {
    val startGame = new Game(
      Array(1, 2, 3, 4, 5, 6, 7),
      Array(14, 9, 10, 11, 12, 13, 14), Player.Second)
    val expectedGame = new Game(
      Array(2, 3, 4, 5, 6, 7, 7),
      Array(1, 11, 11, 12, 13, 14, 15), Player.First)
    expectResult(expectedGame) {
      startGame.play(0)._1
    }
  }
  
  "capture" should "be done when last slot is empty" in {
    val startGame = new Game(
      Array(1, 2, 3, 4, 5, 6, 7),
      Array(2, 9, 0, 11, 12, 13, 14), Player.Second)
    val expectedGame = new Game(
      Array(1, 2, 3, 0, 5, 6, 7),
      Array(0, 10, 0, 11, 12, 13, 19), Player.First)
    expectResult(expectedGame) {
      startGame.play(0)._1
    }
  }
}