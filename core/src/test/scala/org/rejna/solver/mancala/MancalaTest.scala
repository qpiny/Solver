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
  
  it should "implement premature end of game" in {
    val starGame = new Game(
        Array(0, 0, 0, 0, 1, 0, 23), 
        Array(1, 0, 0, 2, 0, 3, 18), SecondPlayer)
    val expectedGame = new Game(
        Array(0, 0, 0, 0, 0, 0, 26),
        Array(0, 0, 0, 0, 0, 0, 22), SecondPlayer)
    expectResult((expectedGame, Action.FirstWin)) {
      starGame.play(5)
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
    val gameStat = new GameStat(FirstPlayer,
      new PlayerStat(10, false,
        Stat(30, 17),
        Stat(26, 30),
        Stat(28, 8)),
      new PlayerStat(16, false,
        Stat(29, 18),
        Stat(27, 31),
        Stat(12, 9)))
    
    val expectedGameStat = new GameStat(FirstPlayer,
        new PlayerStat(10, false,
        Stat(31, 11),
        Stat(26, 30),
        Stat(28, 8)),
      new PlayerStat(16, false,
        Stat(29, 18),
        Stat(27, 31),
        Stat(45, 5)))
    
    expectResult(expectedGameStat) {
      gameStat.update(new GameStat(FirstPlayer,
        new PlayerStat(0, false,
	        Stat(31, 10),
	        Stat(0, 0),
	        Stat(5, 54)),
        new PlayerStat(0, false,
	        Stat(0, 0),
	        Stat(0, 0),
	        Stat(45, 4))))
    }
  }
}

object MancalaCheck extends Properties("GameStat") {
  val gen4 = Gen.choose(0, (1 << 4) - 1)
  val gen6 = Gen.choose(0, (1 << 6) - 1)
  val gen7 = Gen.choose(0, (1 << 7) - 1)
  val gen23 = Gen.choose(0, (1 << 23) - 1)

  val genStat = for {
    score <- gen6
    depth <- gen7
  } yield (Stat(score, depth))

  val genPlayerStat = for {
    winnerCount <- gen23
    winnerPath <- Arbitrary.arbBool.arbitrary
    maxScore <- genStat
    maxDepth <- genStat
    minDepth <- genStat
  } yield new PlayerStat(winnerCount, winnerPath, maxScore, maxDepth, minDepth)

  val genPlayer = Gen.oneOf(FirstPlayer, SecondPlayer)

  val genGameStat = for {
    player <- genPlayer
    firstPlayer <- genPlayerStat
    secondPlayer <- genPlayerStat
  } yield new GameStat(player, firstPlayer, secondPlayer)

  val genGame = for {
    firstBeads <- Gen.listOfN(6, gen4)
    firstScore <- gen7
    secondBeads <- Gen.listOfN(6, gen4)
    secondScore <- gen7
    player <- genPlayer
  } yield new Game(
    (firstBeads :+ firstScore).toArray,
    (secondBeads :+ secondScore).toArray, player)

  val statProp = Prop.forAll(genStat)(s => {
    val bsw = new BitStreamWriter
    s.pushInBitStream(bsw)
    val data = bsw.toByteArray
    val bsr = new BitStreamReader(data: _*)
    data.length == 2 && s == Stat(bsr)
  })
  
  val playerStatProp = Prop.forAll(genPlayerStat)(ps => {
    val bsw = new BitStreamWriter
    ps.pushInBitStream(bsw)
    val data = bsw.toByteArray
    val bsr = new BitStreamReader(data: _*)
    data.length == 8 && ps == new PlayerStat(bsr)
  })
  
  val gameStatProp = Prop.forAll(genGameStat)(gs => {
    val bsw = new BitStreamWriter
    gs.pushInBitStream(bsw)
    val data = bsw.toByteArray
    val bsr = new BitStreamReader(data: _*)
    data.length == 16 && gs == new GameStat(bsr)
  })

  statProp.check(new Parameters.Default {
    override val minSuccessfulTests = 1000
  })
  playerStatProp.check(new Parameters.Default {
    override val minSuccessfulTests = 1000
  })
  gameStatProp.check(new Parameters.Default {
    override val minSuccessfulTests = 1000
  })
}


/*


Player : SecondPlayer
[23] 0 1 0 0 0 0 
     1 0 0 2 0 3 [18]

[0] [3] [5] 
Choice : 5
Tie game
* 
* 
* 
*/
