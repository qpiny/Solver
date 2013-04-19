package org.rejna.solver.mancala

import org.scalatest._
import org.scalacheck._
import org.scalacheck.Test.Parameters

import org.rejna.util.{ BitStreamReader, BitStreamWriter }

class MancalaTest extends FlatSpec {
  "Game board" should "have correctly connected bean for first player" in {
    val startGame = new Game(
      Array(14, 1, 2, 3, 4, 5, 6),
      Array(7, 8, 9, 10, 11, 12, 13), FirstPlayer)
    val expectedGame = new Game(
      Array(1, 3, 3, 4, 5, 6, 7),
      Array(8, 9, 10, 11, 12, 13, 13), SecondPlayer)
    expectResult((expectedGame, Action.NextPlayer)) {
      startGame.play(0)
    }
  }

  it should "have correctly connected bean for second player" in {
    val startGame = new Game(
      Array(1, 2, 3, 4, 5, 6, 7),
      Array(14, 9, 10, 11, 12, 13, 14), SecondPlayer)
    val expectedGame = new Game(
      Array(2, 3, 4, 5, 6, 7, 7),
      Array(1, 11, 11, 12, 13, 14, 15), FirstPlayer)
    expectResult((expectedGame, Action.NextPlayer)) {
      startGame.play(0)
    }
  }

  "Capture" should "be done when last slot is empty" in {
    val startGame = new Game(
      Array(1, 2, 3, 4, 5, 6, 7),
      Array(2, 9, 0, 11, 12, 13, 14), SecondPlayer)
    val expectedGame = new Game(
      Array(1, 2, 3, 0, 5, 6, 7),
      Array(0, 10, 0, 11, 12, 13, 19), FirstPlayer)
    expectResult((expectedGame, Action.NextPlayer)) {
      startGame.play(0)
    }
  }

  it should "not be done on score slot" in {
    val startGame = new Game(
      Array(1, 2, 3, 4, 5, 6, 7),
      Array(6, 9, 10, 11, 12, 13, 0), SecondPlayer)
    val expectedGame = new Game(
      Array(1, 2, 3, 4, 5, 6, 7),
      Array(0, 10, 11, 12, 13, 14, 1), SecondPlayer)
    expectResult((expectedGame, Action.PlayAgain)) {
      startGame.play(0)
    }
  }

  "GameStat" should "be updated with other GameStat" in {
    val gameStat = new GameStat(
      init = true,
      fmaxScoreScore = 30,
      fmaxScoreDepth = 17,
      fmaxDepthScore = 26,
      fmaxDepthDepth = 30,
      fminDepthScore = 28,
      fminDepthDepth = 8,
      fwinnerPath = false,
      fwinnerCount = 10,

      smaxScoreScore = 29,
      smaxScoreDepth = 18,
      smaxDepthScore = 27,
      smaxDepthDepth = 31,
      sminDepthScore = 12,
      sminDepthDepth = 9,
      swinnerPath = false,
      swinnerCount = 16,
      player = FirstPlayer)
    
    val expectedGameStat = new GameStat(
      init = true,
      fmaxScoreScore = 31,
      fmaxScoreDepth = 11,
      fmaxDepthScore = 26,
      fmaxDepthDepth = 30,
      fminDepthScore = 28,
      fminDepthDepth = 8,
      fwinnerPath = false,
      fwinnerCount = 10,

      smaxScoreScore = 29,
      smaxScoreDepth = 18,
      smaxDepthScore = 27,
      smaxDepthDepth = 31,
      sminDepthScore = 45,
      sminDepthDepth = 5,
      swinnerPath = false,
      swinnerCount = 16,
      player = FirstPlayer)
    
    expectResult(expectedGameStat) {
      gameStat.update(new GameStat(
          init = true,
          fmaxScoreScore = 31,
          fmaxScoreDepth = 10,
          fminDepthDepth = 54,
          fminDepthScore = 5,

          sminDepthDepth = 4,
          sminDepthScore = 45
          ))
    }
  }
}

object MancalaCheck extends Properties("GameStat") {
  val gen4 = Gen.choose(0, (1 << 4) - 1)
  val gen6 = Gen.choose(0, (1 << 6) - 1)
  val gen7 = Gen.choose(0, (1 << 7) - 1)
  //val gen8 = Gen.choose(0, (1 << 8) - 1)
  val gen24 = Gen.choose(0, (1 << 24) - 1)
  val genGameStat = for {
    fmaxScoreScore <- gen6
    fmaxScoreDepth <- gen7
    fmaxDepthScore <- gen6
    fmaxDepthDepth <- gen7
    fminDepthScore <- gen6
    fminDepthDepth <- gen7
    fwinnerPath <- Arbitrary.arbBool.arbitrary
    fwinnerCount <- gen24

    smaxScoreScore <- gen6
    smaxScoreDepth <- gen7
    smaxDepthScore <- gen6
    smaxDepthDepth <- gen7
    sminDepthScore <- gen6
    sminDepthDepth <- gen7
    swinnerPath <- Arbitrary.arbBool.arbitrary
    swinnerCount <- gen24
  } yield new GameStat(
    true,
    fmaxScoreScore,
    fmaxScoreDepth,
    fmaxDepthScore,
    fmaxDepthDepth,
    fminDepthScore,
    fminDepthDepth,
    fwinnerPath,
    fwinnerCount,

    smaxScoreScore,
    smaxScoreDepth,
    smaxDepthScore,
    smaxDepthDepth,
    sminDepthScore,
    sminDepthDepth,
    swinnerPath,
    swinnerCount)

  val genGame = for {
    firstBeads <- Gen.listOfN(6, gen4)
    firstScore <- gen7
    secondBeads <- Gen.listOfN(6, gen4)
    secondScore <- gen7
    player <- Gen.oneOf(FirstPlayer, SecondPlayer)
  } yield new Game(
    (firstBeads :+ firstScore).toArray,
    (secondBeads :+ secondScore).toArray, player)

  val gameStatProp = Prop.forAll(genGameStat)(gs => {
    val bsw = new BitStreamWriter
    gs.pushInBitStream(bsw)
    val bsr = new BitStreamReader(bsw.toByteArray: _*)
    gs == new GameStat(bsr)
  })

  gameStatProp.check(new Parameters.Default {
    override val minSuccessfulTests = 1000
  })
}