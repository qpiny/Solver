package org.rejna.solver.mancala

import scala.collection.mutable.WrappedArray
import sbinary._
import org.rejna.solver.{ Node, TreeCompanion, LoggingClass }
import org.rejna.solver.serializer.{ SolverMessage, SolverProtocol }
import org.rejna.util.{ BitStreamReader, BitStreamWriter }

import org.rejna.solver.mancala.Action._

object Game extends TreeCompanion[Game] with SolverMessage with LoggingClass {
  val rootNode = new Game()
  val maxChildren = 6
  val entryLength = 8 // (4 bits * 6 slots + 8 bits * 1 totalSlot) * 2 players

  val GameFormat = new Format[Game] {
    def getBeads(bs: BitStreamReader) = Array.fill[Int](6)(bs.get(4)) :+ bs.get(8)

    def reads(in: Input): Game = {
      val buffer = Array.ofDim[Byte](entryLength)
      in.readFully(buffer)
      val player = if ((buffer(3) & 0x80) != 0) {
        buffer(3) = (buffer(3) & 0x7f).toByte
        SecondPlayer
      } else FirstPlayer
      val bs = new BitStreamReader(buffer: _*)
      new Game(getBeads(bs), getBeads(bs), player)
    }

    def writes(out: Output, game: Game) = {
      var overflow = false
      val bs = new BitStreamWriter
      for (p <- Player) {
        for (s <- (0 to 5)) {
          val nbeads = game.slotValue(p, s)
          val n = if (nbeads > 15) {
            if (overflow) log.error(s"Double overflow in slots : ${game}")
            else overflow = true
            15
          } else
            nbeads
          bs.add(n, 4)
        }
        bs.add(game.slotValue(p, 6), 8)
      }
      val data = bs.toByteArray
      if (game.player == SecondPlayer)
        data(3) = (data(3) | 0x80).toByte
      out.writeAll(data)
    }
  }

  def registerSerializer =
    SolverProtocol.registerFormat(classOf[Game], GameFormat)
}

class Game(first_beads: Array[Int], second_beads: Array[Int], var player: Player = FirstPlayer) extends Node {
  type Self = Game
  /* init slots */
  private val slots = Array.ofDim[Slot](2, 7)

  slots(0)(6) = new ScoreSlot(FirstPlayer, first_beads(6))
  slots(1)(6) = new ScoreSlot(SecondPlayer, second_beads(6))
  slots(0)(6).oppositeSlot = slots(1)(6)
  slots(1)(6).oppositeSlot = slots(0)(6)

  for (i <- (5 to 0 by -1)) {
    slots(0)(i) = new Slot(FirstPlayer, first_beads(i))
    slots(1)(i) = new Slot(SecondPlayer, second_beads(i))
    slots(0)(i).nextSlot = slots(0)(i + 1)
    slots(1)(i).nextSlot = slots(1)(i + 1)
  }
  (0 to 5).foreach(i => {
    slots(0)(i).oppositeSlot = slots(1)(5 - i)
    slots(1)(5 - i).oppositeSlot = slots(0)(i)
  })

  slots(0)(6).nextSlot = slots(1)(0)
  slots(1)(6).nextSlot = slots(0)(0)

  def this(g: Game) = this(g.slots(0).map(_.nbeads), g.slots(1).map(_.nbeads), g.player)
  def this(s: Array[Array[Int]]) = this(s(0), s(1))
  def this(s: Array[Int]) = this(s.take(7), s.takeRight(7))
  def this(i: Int) = this(Array(i, i, i, i, i, i, 0), Array(i, i, i, i, i, i, 0))
  def this() = this(4)

  def doPlay(i: Int): Action = {
    if (i < 0 || i > 5)
      return InvalidMove

    var slot = slots(player.id)(i)

    if (slot.nbeads == 0)
      return InvalidMove

    val nbeads = slot.empty
    for (i <- (0 until nbeads)) {
      slot = slot.nextSlot(player)
      slot.nbeads += 1
    }

    /* capture */
    if (slot.nbeads == 1 && slot.owner == player && !slot.isInstanceOf[ScoreSlot] && slot.oppositeSlot.nbeads != 0) {
      slots(player.id)(6).nbeads += slot.oppositeSlot.empty + slot.empty
    }

    /* game over ? */
    for (p <- Player) {
      if (!slots(p.id).exists(s => s.nbeads > 0 && !s.isInstanceOf[ScoreSlot])) {
        slots(p.other.id)(6).nbeads = slots(p.other.id).foldLeft(0)((score, slot) => score + slot.empty)
        return winner
      }
    }

    /* play again */
    if (slot.isInstanceOf[ScoreSlot])
      PlayAgain
    else {
      player = player.other
      NextPlayer
    }
  }

  def isGameOver = !slots.exists(_.dropRight(1).exists(_.nbeads > 0))

  def winner: Action = {
    val s = score
    if (s._1 > s._2)
      FirstWin
    else if (s._1 < s._2)
      SecondWin
    else
      TieGame
  }

  def slotValue(player: Player, s: Int) = slots(player.id)(s).nbeads

  def score = (slots(FirstPlayer.id)(6).nbeads, slots(SecondPlayer.id)(6).nbeads)

  def play(i: Int): (Game, Action) = {
    val g = new Game(this)
    (g, g.doPlay(i))
  }

  val maxChildren = 6

  def children = {
    (0 until 6).map(i => {
      val g = play(i)
      if (g._2 == InvalidMove)
        None
      else
        Some(g._1)
    }).toArray
  }

  /* return gameStat */
  override def getNodeCompute = {
    if (isGameOver) {
      val (f, s) = score
      new GameStat(player, f, s)
    } else
      new GameStat

  }

  override def toString = {
    val sb = new StringBuffer("Player : ").append(player)
    sb.append("\n[%1$02d] ".format(slots(0)(6).nbeads))
    (5 to 0 by -1).foreach(i => sb.append(slots(0)(i).nbeads).append(" "))
    sb.append("\n     ")
    (0 to 5).foreach(i => sb.append(slots(1)(i).nbeads).append(" "))
    sb.append("[%1$02d]".format(slots(1)(6).nbeads))
    sb.append("\n")
    //sb.append(children.zipWithIndex.filter(_._1.isDefined).map(_._2).mkString("(", ",", ")"))
    sb.toString
  }

  override def hashCode = (slots(player.id) ++ slots(player.other.id)).hashCode

  override def equals(obj: Any): Boolean = {
    obj match {
      case g: Game => 
        g.slots(g.player.id).deep == slots(g.player.id).deep &&
        g.slots(g.player.other.id).deep == slots(g.player.other.id).deep
      case _: Any => false
    }
  }
}