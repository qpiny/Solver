package org.rejna.solver.mancala

import scala.collection.mutable.WrappedArray
import sbinary._
import org.rejna.solver.{ Node, TreeCompanion, LoggingClass }
import org.rejna.solver.serializer.{ SolverMessage, SolverProtocol }
import org.rejna.util.{ BitStreamReader, BitStreamWriter }

import org.rejna.solver.mancala.Player._
import org.rejna.solver.mancala.Action._

object Game extends TreeCompanion[Game] with SolverMessage with LoggingClass {
  val rootNode = new Game()
  val maxChildren = 6
  val entryLength = 8 // (4 bits * 6 slots + 8 bits * 1 totalSlot) * 2 players

  val GameFormat = new Format[Game] {
    def reads(in: Input): Game = {
      val buffer = Array.ofDim[Byte](entryLength)
      in.readFully(buffer)
      val bs = new BitStreamReader(buffer: _*)
      def getBeads = {
        val a = ((0 to 5) map { x => bs.get(4) })
        (a :+ bs.get(8)).toArray
      }
      new Game(getBeads, getBeads)
    }

    def writes(out: Output, game: Game) = {
      var overflow = false
      val bs = new BitStreamWriter

      (0 to 1) map { p =>
        val player = game.player.id ^ p
        (0 to 5) map { i =>
          val nbeads = game.slots(player)(i).nbeads
          val n = if (nbeads > 15) {
            if (overflow) {
              log.error(s"Double overflow in slots : ${game}")
            } else
              overflow = true
            15
          }
          else {
            nbeads
          }
          bs.add(n, 4)
        }
        bs.add(game.slots(player)(6).nbeads, 8)
      }
      out.writeAll(bs.toByteArray)
    }
  }

  def registerSerializer =
    SolverProtocol.registerFormat(classOf[Game], GameFormat)
}

class Game(first_beads: Array[Int], second_beads: Array[Int], var player: Player = First) extends Node {
  type Self = Game
  /* init slots */
  val slots = Array.ofDim[Slot](2, 7)

  (0 to 6).foreach(i => {
    val (fs, ss) = if (i == 6) {
      val s = (new ScoreSlot(First, first_beads(i)), new ScoreSlot(Second, second_beads(i)))
      s._1.oppositeSlot = s._1
      s._2.oppositeSlot = s._2
      s
    } else
      (new Slot(First, first_beads(i)), new Slot(Second, second_beads(i)))
    if (i != 0) {
      slots(0)(i - 1).nextSlot_=(fs)
      slots(1)(i - 1).nextSlot_=(ss)
    }
    slots(0)(i) = fs
    slots(1)(i) = ss
  })
  (0 to 5).foreach(i => {
    slots(0)(i).oppositeSlot = slots(1)(5 - i)
    slots(1)(5 - i).oppositeSlot = slots(0)(i)
  })

  slots(0)(6).nextSlot_=(slots(1)(0))
  slots(1)(6).nextSlot_=(slots(0)(0))

  def this(g: Game) = this(g.slots(0).map(_.nbeads), g.slots(1).map(_.nbeads), g.player)
  def this(s: Array[Array[Int]]) = this(s(0), s(1))
  def this(s: Array[Int]) = this(s.take(7), s.takeRight(7))
  def this(i: Int) = this(Array(i, i, i, i, i, i, 0), Array(i, i, i, i, i, i, 0))
  def this() = this(4)

  def do_play(i: Int): Action = {
    var slot = slots(player.id)(i)

    if (slot.nbeads == 0)
      return InvalidMove

    (0 until slot.empty).foreach(i => {
      slot = slot.nextSlot(player)
      slot.nbeads += 1
    })

    /* capture */
    if (slot.nbeads == 1 && slot.owner == player && !slot.isInstanceOf[ScoreSlot] && slot.oppositeSlot.nbeads != 0)
      slots(player.id)(6).nbeads += slot.oppositeSlot.empty + slot.empty

    /* game over ? */
    (0 to 1).foreach(i => {
      if (!slots(i).exists(s => s.nbeads > 0 && !s.isInstanceOf[ScoreSlot])) {
        slots(i ^ 1)(6).nbeads = slots(i ^ 1).foldLeft(0)((score, slot) => score + slot.empty)
        return winner
      }
    })

    /* play again */
    if (slot.isInstanceOf[ScoreSlot])
      PlayAgain
    else {
      player = Player(player.id ^ 1)
      NextPlayer
    }
  }

  def winner: Action = {
    if (slots(0)(6).nbeads > slots(1)(6).nbeads)
      FirstWin
    else if (slots(0)(6).nbeads < slots(1)(6).nbeads)
      SecondWin
    else
      TieGame
  }

  def score = (slots(0)(6).nbeads, slots(1)(6).nbeads)

  def play(i: Int): (Game, Action) = {
    val g = new Game(this)
    (g, g.do_play(i))
  }

  val maxChildren = 6

  def children = {
    (0 until 6)
      .map(i => {
        val g = play(i)
        if (g._2 == InvalidMove)
          None
        else
          Some(g._1)
      }).toArray
  }

  /* return gameStat */
  override def getValue = {
    val (f, s) = score
    new GameStat(player, f, s)
  }

  override def toString = {
    val sb = new StringBuffer("Player : ").append(player)
    sb.append("\n[%1$02d] ".format(slots(0)(6).nbeads))
    (5 to 0 by -1).foreach(i => sb.append(slots(0)(i).nbeads).append(" "))
    sb.append("\n     ")
    (0 to 5).foreach(i => sb.append(slots(1)(i).nbeads).append(" "))
    sb.append("[%1$02d]".format(slots(1)(6).nbeads))
    sb.append("\n")
    sb.append(children.zipWithIndex.filter(_._1.isDefined).map(_._2).mkString("(", ",", ")"))
    sb.toString
  }

  override def hashCode = slots.hashCode

  override def equals(obj: Any): Boolean = {
    if (!obj.isInstanceOf[Game])
      return false
    val g = obj.asInstanceOf[Game]
    g.slots(g.player.id).deep == slots(player.id).deep &&
      g.slots(g.player.id ^ 1).deep == slots(player.id ^ 1).deep
  }
}