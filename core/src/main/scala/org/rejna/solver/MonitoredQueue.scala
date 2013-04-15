package org.rejna.solver

import java.util.Collection
import java.util.concurrent.{ TimeUnit, BlockingQueue }

import akka.actor.ActorRef
import akka.dispatch.{ MessageQueue, Envelope }

class MonitoredThreadQueue(name: String, queue: BlockingQueue[Runnable]) extends BlockingQueue[Runnable] {
  val monitoredSize = PerfCounter(DefaultSystem.system).getVariable(s"${name}.queue")
  
  def add(r: Runnable) = {
    queue.add(r)
    monitoredSize.inc()
    true
  }
  def offer(r: Runnable) = {
    if (queue.add(r)) { monitoredSize.inc(); true }
    else false
  }
  def put(r: Runnable) = { queue.put(r); monitoredSize.inc() }
  def offer(r: Runnable, timeout: Long, unit: TimeUnit) = {
    if (queue.offer(r, timeout, unit)) { monitoredSize.inc(); true }
    else false
  }
  def take = {
    val r = queue.take()
    if (r != null) monitoredSize.dec()
    r
  }
  def poll = {
    val r = queue.poll()
    if (r != null) monitoredSize.dec()
    r
  }
  def poll(timeout: Long, unit: TimeUnit) = {
    val r = queue.poll(timeout, unit)
    if (r != null) monitoredSize.dec()
    r
  }
  def remainingCapacity = queue.remainingCapacity
  def remove = {
    var r = queue.remove()
    monitoredSize.dec()
    r
  }
  def remove(o: AnyRef) = {
    if (queue.remove(o)) {
      monitoredSize.dec()
      true
    }
    else
      false
      
  }
  def contains(o: AnyRef) = queue.contains(o) 
  def drainTo(c: Collection[_ >: Runnable]) = {
    var r = queue.drainTo(c)
    monitoredSize.dec(r)
    r
  }
  def drainTo(c: Collection[_ >: Runnable], maxElements: Int) = {
    var r = queue.drainTo(c, maxElements)
    monitoredSize.dec(r)
    r
  }
  def addAll(c: Collection[_ <: Runnable]) = {
    val r = queue.addAll(c)
    monitoredSize.inc(c.size)
    r
  }
  def clear = { queue.clear; monitoredSize.set(0) }
  def containsAll(c: Collection[_]) = { queue.containsAll(c) }
  def isEmpty = { queue.isEmpty }
  def iterator = { queue.iterator }
  def removeAll(c: Collection[_]) = {
    var r = queue.removeAll(c)
    monitoredSize.set(queue.size)
    r
  }
  def retainAll(c: Collection[_]) = {
    val r = queue.retainAll(c)
    monitoredSize.set(queue.size)
    r
  }
  def size = { queue.size }
  def toArray[T](a: Array[T with Object]) = queue.toArray[T](a)
  def toArray() = queue.toArray()
  def element() = queue.element
  def peek = queue.peek  
}

class MonitoredMailQueue(val name: String, queue: MessageQueue) extends MessageQueue with NamedMailQueue {
  val monitoredSize = PerfCounter(DefaultSystem.system).getVariable(s"${name}.mailbox")

  def enqueue(receiver: ActorRef, handle: Envelope) = {
    val e = queue.enqueue(receiver: ActorRef, handle: Envelope)
    monitoredSize.inc()
    e
  }

  def dequeue() = {
    val e = queue.dequeue()
    if (e != null)
      monitoredSize.dec()
    e
  }
  
  def numberOfMessages = queue.numberOfMessages

  def hasMessages = queue.hasMessages

  def cleanUp(owner: ActorRef, deadLetters: MessageQueue) = {
    queue.cleanUp(owner, deadLetters)
    monitoredSize.set(queue.numberOfMessages)
  }
}

trait NamedMailQueue { val name: String }

class MonitoredPrioBlockingQueue(name: String) extends PrioBlockinkQueue {
  val monitoredPrio = PerfCounter(DefaultSystem.system).getVariable(s"${name}.prio")
  
  private def update(r: Runnable): Runnable = {
    r.getClass.getMethod("messageQueue").invoke(r) match {
	    case mmq: MonitoredMailQueue => monitoredPrio.set(mmq.name.length)
	    case _ => 
    }
    r
  }
  override def add(r: Runnable) = super.add(update(r))
  override def offer(r: Runnable) = super.offer(update(r))
  override def put(r: Runnable) = super.put(update(r))
  override def offer(r: Runnable, timeout: Long, unit: TimeUnit) = super.offer(update(r), timeout, unit)
}
