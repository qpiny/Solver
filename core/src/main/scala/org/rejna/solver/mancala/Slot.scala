package org.rejna.solver.mancala

import scala.concurrent.{ Promise, Await }
import scala.concurrent.duration._
import scala.language.postfixOps

class Slot(val owner: Player, var nbeads: Int) extends Serializable {
  private val nextSlotPromise = Promise[Slot]
  private lazy val _nextSlot: Slot = Await.result(nextSlotPromise.future, 10 seconds)
  private val oppositeSlotPromise = Promise[Slot]
  private lazy val _oppositeSlot: Slot = Await.result(oppositeSlotPromise.future, 10 seconds)
  private lazy val lastSlot: Boolean = _nextSlot.isInstanceOf[ScoreSlot]

  def nextSlot_=(slot: Slot) = nextSlotPromise.success(slot)

  def nextSlot = sys.error("not supported")
  
  def nextSlot(player: Player) = {
    if (lastSlot && player != owner)
      _nextSlot._nextSlot
    else
      _nextSlot
  }

  def oppositeSlot_=(slot: Slot) = {
    oppositeSlotPromise.success(slot)
  }

  def oppositeSlot = _oppositeSlot

  def empty = {
    val v = nbeads
    nbeads = 0
    v
  }

  override def equals(obj: Any) = {
    obj.isInstanceOf[Slot] && (obj.asInstanceOf[Slot].nbeads == this.nbeads)
  }

  override def hashCode = nbeads.hashCode
  
  override def toString = nbeads.toString
}

class ScoreSlot(owner: Player, nbeads: Int) extends Slot(owner, nbeads)

