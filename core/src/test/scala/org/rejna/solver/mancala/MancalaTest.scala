package org.rejna.solver.mancala

import org.scalatest._
import org.scalacheck._

class MancalaTest extends FlatSpec {
  "Game board" should "have correctly connected bean for first player" in {
    val startGame = new Game(
      Array(14, 1, 2, 3, 4, 5, 6),
      Array(7, 8, 9, 10, 11, 12, 13), Player.First)
    val expectedGame = new Game(
      Array(1, 3, 3, 4, 5, 6, 7),
      Array(8, 9, 10, 11, 12, 13, 13), Player.Second)
    expectResult((expectedGame, Action.NextPlayer)) {
      startGame.play(0)
    }
  }

  it should "have correctly connected bean for second player" in {
    val startGame = new Game(
      Array(1, 2, 3, 4, 5, 6, 7),
      Array(14, 9, 10, 11, 12, 13, 14), Player.Second)
    val expectedGame = new Game(
      Array(2, 3, 4, 5, 6, 7, 7),
      Array(1, 11, 11, 12, 13, 14, 15), Player.First)
    expectResult((expectedGame, Action.NextPlayer)) {
      startGame.play(0)
    }
  }

  "Capture" should "be done when last slot is empty" in {
    val startGame = new Game(
      Array(1, 2, 3, 4, 5, 6, 7),
      Array(2, 9, 0, 11, 12, 13, 14), Player.Second)
    val expectedGame = new Game(
      Array(1, 2, 3, 0, 5, 6, 7),
      Array(0, 10, 0, 11, 12, 13, 19), Player.First)
    expectResult((expectedGame, Action.NextPlayer)) {
      startGame.play(0)
    }
  }

  it should "not be done on score slot" in {
    val startGame = new Game(
      Array(1, 2, 3, 4, 5, 6, 7),
      Array(6, 9, 10, 11, 12, 13, 0), Player.Second)
    val expectedGame = new Game(
      Array(1, 2, 3, 0, 5, 6, 7),
      Array(0, 10, 11, 12, 13, 14, 1), Player.First)
    expectResult((expectedGame, Action.PlayAgain)) {
      startGame.play(0)._1
    }
  }
}

object MancalaCheck extends Properties("GameStat") {
  /*
  fmax_score_score = bs.get(6)
    fmax_score_depth = bs.get(7)
    fmax_depth_score = bs.get(6)
    fmax_depth_depth = bs.get(7)
    fmin_depth_score = bs.get(6)
    fmin_depth_depth = bs.get(7)
    fwinner_path = bs.get(1) == 1
    fwinner_count = bs.get(24)

    smax_score_score = bs.get(6)
    smax_score_depth = bs.get(7)
    smax_depth_score = bs.get(6)
    smax_depth_depth = bs.get(7)
    smin_depth_score = bs.get(6)
    smin_depth_depth = bs.get(7)
    swinner_path = bs.get(1) == 1
    swinner_count = bs.get(24)
*/

  val gen6 = Gen.choose(0, (1 << 6) - 1)
  val gen7 = Gen.choose(0, (1 << 7) - 1)
  val gen24 = Gen.choose(0, (1 << 24) - 1)
  //Prop.forAll(
  for {
    fmax_score_score <- gen6
    fmax_score_depth <- gen7
    fmax_depth_score <- gen6
    fmax_depth_depth <- gen7
    fmin_depth_score <- gen6
    fmin_depth_depth <- gen7
    fwinner_path <- Arbitrary.arbBool.arbitrary
    fwinner_count <- gen24

    smax_score_score <- gen6
    smax_score_depth <- gen7
    smax_depth_score <- gen6
    smax_depth_depth <- gen7
    smin_depth_score <- gen6
    smin_depth_depth <- gen7
    swinner_path <- Arbitrary.arbBool.arbitrary
    swinner_count <- gen24
  } yield true 
}