package org.rejna.solver.mancala

import sbinary._
import org.rejna.solver.NodeValue
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

class GameStat(
  var init: Boolean = false,
  var fmaxScoreScore: Int = 0,
  var fmaxScoreDepth: Int = 0,
  var fmaxDepthScore: Int = 0,
  var fmaxDepthDepth: Int = 0,
  var fminDepthScore: Int = 0,
  var fminDepthDepth: Int = 0,
  var fwinnerPath: Boolean = false,
  var fwinnerCount: Int = 0,
  var smaxScoreScore: Int = 0,
  var smaxScoreDepth: Int = 0,
  var smaxDepthScore: Int = 0,
  var smaxDepthDepth: Int = 0,
  var sminDepthScore: Int = 0,
  var sminDepthDepth: Int = 0,
  var swinnerPath: Boolean = false,
  var swinnerCount: Int = 0,
  var player: Player = FirstPlayer) extends NodeValue {

  def this(bs: BitStreamReader) =
    this(init = true,
      fmaxScoreScore = bs.get(6),
      fmaxScoreDepth = bs.get(7),
      fmaxDepthScore = bs.get(6),
      fmaxDepthDepth = bs.get(7),
      fminDepthScore = bs.get(6),
      fminDepthDepth = bs.get(7),
      fwinnerPath = bs.get(1) == 1,
      fwinnerCount = bs.get(24),

      smaxScoreScore = bs.get(6),
      smaxScoreDepth = bs.get(7),
      smaxDepthScore = bs.get(6),
      smaxDepthDepth = bs.get(7),
      sminDepthScore = bs.get(6),
      sminDepthDepth = bs.get(7),
      swinnerPath = bs.get(1) == 1,
      swinnerCount = bs.get(24),
      player = FirstPlayer)

  def this(bin: Array[Byte]) = {
    this(new BitStreamReader(bin: _*))
  }

  def this(p: Player, first: Int, second: Int) {
    this(player = p)
    if (first + second == 48) {
      init = true
      fmaxScoreScore = first
      fmaxDepthScore = first
      fminDepthScore = first
      smaxScoreScore = second
      smaxDepthScore = second
      sminDepthScore = second
      if (first > second) {
        fwinnerPath = true
        fwinnerCount = 1
      } else {
        swinnerPath = true
        swinnerCount = 1
      }
    }
  }

  def pushInBitStream(bs: BitStreamWriter): BitStreamWriter = { /* 16 bytes long */
    bs.add(fmaxScoreScore, 6)
    bs.add(fmaxScoreDepth, 7)
    bs.add(fmaxDepthScore, 6)
    bs.add(fmaxDepthDepth, 7)
    bs.add(fminDepthScore, 6)
    bs.add(fminDepthDepth, 7)
    bs.add(if (fwinnerPath) 1 else 0, 1)
    bs.add(fwinnerCount, 24)
    bs.add(smaxScoreScore, 6)
    bs.add(smaxScoreDepth, 7)
    bs.add(smaxDepthScore, 6)
    bs.add(smaxDepthDepth, 7)
    bs.add(sminDepthScore, 6)
    bs.add(sminDepthDepth, 7)
    bs.add(if (swinnerPath) 1 else 0, 1)
    bs.add(swinnerCount, 24)
    bs
  }

  def update(nv: NodeValue) = {
    val gs = nv.asInstanceOf[GameStat]
    if (gs.init) {
      if (!init) {
        init = true
        fmaxScoreScore = gs.fmaxScoreScore
        fmaxScoreDepth = gs.fmaxScoreDepth + 1
        fmaxDepthScore = gs.fmaxDepthScore
        fmaxDepthDepth = gs.fmaxDepthDepth + 1
        fminDepthScore = gs.fminDepthScore
        fminDepthDepth = gs.fminDepthDepth + 1
        fwinnerPath = gs.fwinnerPath
        fwinnerCount = gs.fwinnerCount
        smaxScoreScore = gs.smaxScoreScore
        smaxScoreDepth = gs.smaxScoreDepth + 1
        smaxDepthScore = gs.smaxDepthScore
        smaxDepthDepth = gs.smaxDepthDepth + 1
        sminDepthScore = gs.sminDepthScore
        sminDepthDepth = gs.sminDepthDepth + 1
        swinnerPath = gs.swinnerPath
        swinnerCount = gs.swinnerCount
      } else {
        if (fmaxScoreScore < gs.fmaxScoreScore) {
          fmaxScoreScore = gs.fmaxScoreScore
          fmaxScoreDepth = gs.fmaxScoreDepth + 1
        }
        if (fmaxDepthDepth < gs.fmaxDepthDepth + 1) {
          fmaxDepthDepth = gs.fmaxDepthDepth + 1
          fmaxDepthScore = gs.fmaxDepthScore
        }
        if (fminDepthDepth > gs.fminDepthDepth + 1) {
          fminDepthDepth = gs.fminDepthDepth + 1
          fminDepthScore = gs.fminDepthScore
        }
        fwinnerCount += gs.fwinnerCount

        if (smaxScoreScore < gs.smaxScoreScore) {
          smaxScoreScore = gs.smaxScoreScore
          smaxScoreDepth = gs.smaxScoreDepth + 1
        }
        if (smaxDepthDepth < gs.smaxDepthDepth + 1) {
          smaxDepthDepth = gs.smaxDepthDepth + 1
          smaxDepthScore = gs.smaxDepthScore
        }
        if (sminDepthDepth > gs.sminDepthDepth + 1) {
          sminDepthDepth = gs.sminDepthDepth + 1
          sminDepthScore = gs.sminDepthScore
        }
        swinnerCount += gs.swinnerCount

        if (player == FirstPlayer) {
          if (gs.fwinnerPath)
            fwinnerPath = true
          if (!gs.swinnerPath)
            swinnerPath = false
        } else {
          if (gs.swinnerPath)
            swinnerPath = true
          if (!gs.fwinnerPath)
            fwinnerPath = false
        }
      }
    }
    this
  }

  override def equals(o: Any) = {
    o match {
      case gs: GameStat =>
        fmaxScoreScore == gs.fmaxScoreScore &&
          fmaxScoreDepth == gs.fmaxScoreDepth &&
          fmaxDepthScore == gs.fmaxDepthScore &&
          fmaxDepthDepth == gs.fmaxDepthDepth &&
          fminDepthScore == gs.fminDepthScore &&
          fminDepthDepth == gs.fminDepthDepth &&
          fwinnerPath == gs.fwinnerPath &&
          fwinnerCount == gs.fwinnerCount &&
          smaxScoreScore == gs.smaxScoreScore &&
          smaxScoreDepth == gs.smaxScoreDepth &&
          smaxDepthScore == gs.smaxDepthScore &&
          smaxDepthDepth == gs.smaxDepthDepth &&
          sminDepthScore == gs.sminDepthScore &&
          sminDepthDepth == gs.sminDepthDepth &&
          swinnerPath == gs.swinnerPath &&
          swinnerCount == gs.swinnerCount &&
          player == gs.player
      case _: Any => false
    }
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
    val wincount = fwinnerCount + swinnerCount
    val (fwin, swin) = if (wincount != 0) (100 * fwinnerCount / wincount, 100 * swinnerCount / wincount)
    else (-1, -1)
    ("Player 1             | Player 2            \n" +
      "Win : %3d%% %-7d | Win : %3d%% %-7d\n" +
      "Winner path : %-5s  | Winner path : %-5s\n" +
      "Max score : %-2d (%-3d) | Max score : %-2d (%-3d)\n" +
      "Max depth : %-2d (%-3d) | Max depth : %-2d (%-3d)\n" +
      "Min depth : %-2d (%-3d) | Min depth : %-2d (%-3d)")
      .format(fwin, fwinnerCount, swin, swinnerCount,
        fwinnerPath, swinnerPath,
        fmaxScoreScore, fmaxScoreDepth, smaxScoreScore, smaxScoreDepth,
        fmaxDepthScore, fmaxDepthDepth, smaxDepthScore, smaxDepthDepth,
        fminDepthScore, fminDepthDepth, sminDepthScore, sminDepthDepth)
  }
}