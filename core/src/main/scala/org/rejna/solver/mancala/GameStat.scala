package org.rejna.solver.mancala

import sbinary._
import org.rejna.solver.NodeValue
import org.rejna.solver.NodeCompute
import org.rejna.solver.Node
import org.rejna.solver.TreeCompanion
import org.rejna.solver.serializer.SolverMessage
import org.rejna.solver.serializer.SolverProtocol
import org.rejna.util.{ BitStreamReader, BitStreamWriter }
import org.rejna.solver.mancala.Action._

object GameStat extends TreeCompanion[GameStat] with SolverMessage {
  def rootNode = sys.error("NodeValue has no rootNode")
  def maxChildren = Game.maxChildren
  val entryLength = 16
  val GameStatFormat = new Format[GameStat] {

    def reads(in: Input): GameStat = {
      val data = Array.ofDim[Byte](entryLength)
      in.readFully(data)
      new GameStat(new BitStreamReader(data: _*))
    }

    def writes(out: Output, gameStat: GameStat) = {
      val bs = new BitStreamWriter
      gameStat.pushInBitStream(bs)
      out.writeAll(bs.toByteArray)
    }
  }
  def registerSerializer =
    SolverProtocol.registerFormat(classOf[GameStat], GameStatFormat)
}

case class Stat(score: Int, depth: Int) {
  def pushInBitStream(bs: BitStreamWriter) = {
    bs.add(score, 6)
    bs.add(depth, 7)
  }

  def deeper: Stat = copy(depth = depth + 1)

  def toStr: String = {
    "%-2d (%-3d)".format(score, depth)
  }

}
object Stat {
  def apply(bs: BitStreamReader): Stat = Stat(bs.get(6), bs.get(7))
}

class PlayerStat(
  var winnerCount: Int = 0,
  var winnerPath: Boolean = false,
  var maxScore: Stat = Stat(0, 0),
  var maxDepth: Stat = Stat(0, 0),
  var minDepth: Stat = Stat(0, 0)) {

  def this(bs: BitStreamReader) = this(
    winnerCount = bs.get(23),
    winnerPath = bs.get(1) == 1,
    maxScore = Stat(bs),
    maxDepth = Stat(bs),
    minDepth = Stat(bs))

  def this(winner: Boolean, stat: Stat) = this(if (winner) 1 else 0, winner, stat, stat, stat)

  def pushInBitStream(bs: BitStreamWriter): Unit = { /* 16 bytes long minus 1 bit */
    bs.add(winnerCount, 23)
    bs.add(if (winnerPath) 1 else 0, 1)
    maxScore.pushInBitStream(bs)
    maxDepth.pushInBitStream(bs)
    minDepth.pushInBitStream(bs)
  }

  def update(ps: PlayerStat) = {
    winnerCount = ps.winnerCount
    winnerPath = ps.winnerPath
    maxScore = ps.maxScore
    maxDepth = ps.maxDepth
    minDepth = ps.minDepth
  }

  def deeper: PlayerStat = new PlayerStat(
    winnerCount = winnerCount,
    winnerPath = winnerPath,
    maxScore = maxScore.deeper,
    maxDepth = maxDepth.deeper,
    minDepth = minDepth.deeper)

  override def equals(o: Any) = o match {
    case ps: PlayerStat =>
      winnerCount == ps.winnerCount &&
        winnerPath == ps.winnerPath &&
        maxScore == ps.maxScore &&
        maxDepth == ps.maxDepth &&
        minDepth == ps.minDepth
    case _: Any => false
  }
}

class GameStat(
  val player: Player,
  val firstPlayer: PlayerStat,
  val secondPlayer: PlayerStat) extends NodeValue with NodeCompute {

  var init: Boolean = true

  def this() = {
    this(FirstPlayer, new PlayerStat, new PlayerStat)
    init = false
  }

  def this(bs: BitStreamReader) = {
    this(Player(bs.get(1)), new PlayerStat(bs), { bs.get(1); new PlayerStat(bs) })
  }

  def this(bin: Array[Byte]) = {
    this(new BitStreamReader(bin: _*))
  }

  def this(p: Player, first: Int, second: Int) =
    this(p,
      new PlayerStat(first > second, Stat(first, second)),
      new PlayerStat(second > first, Stat(first, second)))

  def pushInBitStream(bs: BitStreamWriter): Unit = { /* 16 bytes long */
    bs.add(player.id, 1)
    firstPlayer.pushInBitStream(bs)
    bs.add(0, 1) // padding
    secondPlayer.pushInBitStream(bs)
  }

  def update(nc: NodeCompute) = {
    val gs = nc.asInstanceOf[GameStat]
    if (gs.init) {
      if (!init) {
        init = true
        firstPlayer() = gs.firstPlayer.deeper
        secondPlayer() = gs.secondPlayer.deeper
      } else {
        if (firstPlayer.maxScore.score < gs.firstPlayer.maxScore.score)
          firstPlayer.maxScore = gs.firstPlayer.maxScore.deeper
        if (firstPlayer.maxDepth.depth < gs.firstPlayer.maxDepth.depth + 1)
          firstPlayer.maxDepth = gs.firstPlayer.maxDepth.deeper
        if (firstPlayer.minDepth.depth > gs.firstPlayer.minDepth.depth + 1)
          firstPlayer.minDepth = gs.firstPlayer.minDepth.deeper
        firstPlayer.winnerCount += gs.firstPlayer.winnerCount

        if (secondPlayer.maxScore.score < gs.secondPlayer.maxScore.score)
          secondPlayer.maxScore = gs.secondPlayer.maxScore.deeper
        if (secondPlayer.maxDepth.depth < gs.secondPlayer.maxDepth.depth + 1)
          secondPlayer.maxDepth = gs.secondPlayer.maxDepth.deeper
        if (secondPlayer.minDepth.depth > gs.secondPlayer.minDepth.depth + 1)
          secondPlayer.minDepth = gs.secondPlayer.minDepth.deeper
        secondPlayer.winnerCount += gs.secondPlayer.winnerCount

        if (player == FirstPlayer) {
          if (gs.firstPlayer.winnerPath)
            firstPlayer.winnerPath = true
          if (!gs.secondPlayer.winnerPath)
            secondPlayer.winnerPath = false
        } else {
          if (gs.secondPlayer.winnerPath)
            secondPlayer.winnerPath = true
          if (!gs.firstPlayer.winnerPath)
            firstPlayer.winnerPath = false
        }
      }
    }
    this
  }

  def getNodeValue(n: Node) = {
    val game = n.asInstanceOf[Game]
    if (game.player == player)
      this
    else
      new GameStat(game.player, secondPlayer, firstPlayer)
  }

  override def equals(o: Any) = o match {
    case gs: GameStat =>
      player == gs.player &&
        firstPlayer == gs.firstPlayer &&
        secondPlayer == gs.secondPlayer
    case _: Any => false
  }

  override def toString = {
    /*
     * Player 1            | Player 2
     * Win : xxx%          |
     * Winner path : false |
     * Max score : 11 (31) |
     * Max depth : 12 (14) |
     * Min depth : 12 (15) |
     */
    val wincount = firstPlayer.winnerCount + secondPlayer.winnerCount
    val (fwin, swin) = if (wincount != 0) (100 * firstPlayer.winnerCount / wincount, 100 * secondPlayer.winnerCount / wincount)
    else (-1, -1)
    ("Player 1             | Player 2            \n" +
      "Win : %3d%% %-7d | Win : %3d%% %-7d\n" +
      "Winner path : %-5s  | Winner path : %-5s\n" +
      "Max score : %8s | Max score : %8s\n" +
      "Max depth : %8s | Max depth : %8s\n" +
      "Min depth : %8s | Min depth : %8s")
      .format(fwin, firstPlayer.winnerCount, swin, secondPlayer.winnerCount,
        firstPlayer.winnerPath, secondPlayer.winnerPath,
        firstPlayer.maxScore.toStr, secondPlayer.maxScore.toStr,
        firstPlayer.maxDepth.toStr, secondPlayer.maxDepth.toStr,
        firstPlayer.minDepth.toStr, secondPlayer.minDepth.toStr)
  }
}