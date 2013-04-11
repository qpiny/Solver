package org.rejna.solver.mancala

import sbinary._
import org.rejna.solver.NodeValue
import org.rejna.solver.Node
import org.rejna.solver.TreeCompanion
import org.rejna.solver.serializer.SolverMessage
import org.rejna.solver.serializer.SolverProtocol
import org.rejna.util.{ BitStreamReader, BitStreamWriter }
import org.rejna.solver.mancala.Player._
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

class GameStat extends NodeValue {
  var init = false
  var fmax_score_score = 0
  var fmax_score_depth = 0
  var fmax_depth_score = 0
  var fmax_depth_depth = 0
  var fmin_depth_score = 0
  var fmin_depth_depth = 0
  var fwinner_path = false
  var fwinner_count = 0
  var smax_score_score = 0
  var smax_score_depth = 0
  var smax_depth_score = 0
  var smax_depth_depth = 0
  var smin_depth_score = 0
  var smin_depth_depth = 0
  var swinner_path = false
  var swinner_count = 0
  var player: Player = First

  def this(bs: BitStreamReader) = {
    this
    init = true
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
  }

  def this(bin: Array[Byte]) = {
    this(new BitStreamReader(bin: _*))
  }

  def this(p: Player, first: Int, second: Int) {
    this
    player = p
    if (first + second == 48) {
      init = true
      fmax_score_score = first
      fmax_depth_score = first
      fmin_depth_score = first
      smax_score_score = second
      smax_depth_score = second
      smin_depth_score = second
      if (first > second) {
        fwinner_path = true
        fwinner_count = 1
      } else {
        smin_depth_depth = 0
        swinner_path = true
        swinner_count = 1
      }
    }
  }

  def pushInBitStream(bs: BitStreamWriter): BitStreamWriter = { /* 16 bytes long */
    bs.add(fmax_score_score, 6)
    bs.add(fmax_score_depth, 7)
    bs.add(fmax_depth_score, 6)
    bs.add(fmax_depth_depth, 7)
    bs.add(fmin_depth_score, 6)
    bs.add(fmin_depth_depth, 7)
    bs.add(if (fwinner_path) 1 else 0, 1)
    bs.add(fwinner_count, 24)
    bs.add(smax_score_score, 6)
    bs.add(smax_score_depth, 7)
    bs.add(smax_depth_score, 6)
    bs.add(smax_depth_depth, 7)
    bs.add(smin_depth_score, 6)
    bs.add(smin_depth_depth, 7)
    bs.add(if (swinner_path) 1 else 0, 1)
    bs.add(swinner_count, 24)
    bs
  }

  def update(nv: NodeValue) = synchronized {
    val gs = nv.asInstanceOf[GameStat]
    if (gs.init) {
      if (!init) {
        init = true
        fmax_score_score = gs.fmax_score_score
        fmax_score_depth = gs.fmax_score_depth + 1
        fmax_depth_score = gs.fmax_depth_score
        fmax_depth_depth = gs.fmax_depth_depth + 1
        fmin_depth_score = gs.fmin_depth_score
        fmin_depth_depth = gs.fmin_depth_depth + 1
        fwinner_path = gs.fwinner_path
        fwinner_count = gs.fwinner_count
        smax_score_score = gs.smax_score_score
        smax_score_depth = gs.smax_score_depth + 1
        smax_depth_score = gs.smax_depth_score
        smax_depth_depth = gs.smax_depth_depth + 1
        smin_depth_score = gs.smin_depth_score
        smin_depth_depth = gs.smin_depth_depth + 1
        swinner_path = gs.swinner_path
        swinner_count = gs.swinner_count
      } else {
        if (fmax_score_score < gs.fmax_score_score) {
          fmax_score_score = gs.fmax_score_score
          fmax_score_depth = gs.fmax_score_depth + 1
        }
        if (fmax_depth_depth < gs.fmax_depth_depth + 1) {
          fmax_depth_depth = gs.fmax_depth_depth + 1
          fmax_depth_score = gs.fmax_depth_score
        }
        if (fmin_depth_depth > gs.fmin_depth_depth + 1) {
          fmin_depth_depth = gs.fmin_depth_depth + 1
          fmin_depth_score = gs.fmin_depth_score
        }
        fwinner_count += gs.fwinner_count

        if (smax_score_score < gs.smax_score_score) {
          smax_score_score = gs.smax_score_score
          smax_score_depth = gs.smax_score_depth + 1
        }
        if (smax_depth_depth < gs.smax_depth_depth + 1) {
          smax_depth_depth = gs.smax_depth_depth + 1
          smax_depth_score = gs.smax_depth_score
        }
        if (smin_depth_depth > gs.smin_depth_depth + 1) {
          smin_depth_depth = gs.smin_depth_depth + 1
          smin_depth_score = gs.smin_depth_score
        }
        swinner_count += gs.swinner_count

        if (player == First) {
          if (gs.fwinner_path)
            fwinner_path = true
          if (!gs.swinner_path)
            swinner_path = false
        } else {
          if (gs.swinner_path)
            swinner_path = true
          if (!gs.fwinner_path)
            fwinner_path = false
        }
      }
    }
    this
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
    val wincount = fwinner_count + swinner_count
    val (fwin, swin) = if (wincount != 0) (100 * fwinner_count / wincount, 100 * swinner_count / wincount)
    else (-1, -1)
    ("Player 1             | Player 2            \n" +
      "Win : %3d%% %-7d | Win : %3d%% %-7d\n" +
      "Winner path : %-5s  | Winner path : %-5s\n" +
      "Max score : %-2d (%-3d) | Max score : %-2d (%-3d)\n" +
      "Max depth : %-2d (%-3d) | Max depth : %-2d (%-3d)\n" +
      "Min depth : %-2d (%-3d) | Min depth : %-2d (%-3d)")
      .format(fwin, fwinner_count, swin, swinner_count,
        fwinner_path, swinner_path,
        fmax_score_score, fmax_score_depth, smax_score_score, smax_score_depth,
        fmax_depth_score, fmax_depth_depth, smax_depth_score, smax_depth_depth,
        fmin_depth_score, fmin_depth_depth, smin_depth_score, smin_depth_depth)
  }
}