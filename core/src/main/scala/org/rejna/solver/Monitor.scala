package org.rejna.solver

import scala.collection.mutable.HashMap
import scala.concurrent.duration._
import language.postfixOps
import java.util.concurrent.atomic.AtomicLong
import akka.actor._
import com.typesafe.config.{ Config, ConfigFactory }

class Monitor(system: ActorSystem, config: Config) extends Extension with LoggingClass {
  private val counters = HashMap.empty[String, AtomicLong]
  private val gauges = HashMap.empty[String, Gauge]
  private val freq = config.getInt("show-freq") milliseconds

  if (freq.toMillis > 0) {
    implicit val execCtx = system.dispatcher
    system.scheduler.schedule(2 seconds, freq) {
      log.info(toString)
    }
  }

  def incCounter(counterName: String) = getCounter(counterName).incrementAndGet
  def decCounter(counterName: String) = getCounter(counterName).decrementAndGet
  def getCounter(counterName: String) = counters.getOrElseUpdate(counterName, new AtomicLong(0))
  def getCounters = counters.mapValues(_.get)

  def getGauge(varName: String) = gauges.getOrElseUpdate(varName, new Gauge)
  def getGauges = gauges.mapValues(_.get)

  //    counterName match {
  //    case "queue.size" => new AtomicLong(PrioBlockinkQueue.size) //LifoBlockingQueue.size)
  //    case _ => counters.getOrElseUpdate(counterName, new AtomicLong(0))
  //  }
  //  def apply(counterName: String) = get(counterName).get
  override def toString = getCounters.toString + "\n" +
    getGauges.toString
}

object Monitor extends ExtensionId[Monitor] with ExtensionIdProvider {
  override def lookup = Monitor

  override def createExtension(system: ExtendedActorSystem) = {
    val config = if (system.settings.config.hasPath("monitor"))
      system.settings.config.getConfig("monitor")
    else
      ConfigFactory.empty
    new Monitor(system, config)
  }
}

case class MonitoredValue(min: Long, current: Long, max: Long)

class Gauge {
  private var value = 0L
  private var min = 0L
  private var max = 0L

  def inc(v: Long = 1L) = { // should be synchronized {
    value += v
    if (value > max)
      max = value
  }

  def dec(v: Long = 1L) = { // should be synchronized {
    value -= v
    if (value < min)
      min = value
  }

  def set(v: Long) = { // should be synchronized {
    value = v
    if (value > max)
      max = value
    else if (value < min)
      min = value
  }

  def get = { // should be synchronized {
    val r = MonitoredValue(min, value, max)
    min = value
    max = value
    r
  }
  
  def intValue = value.toInt // used only by Cluster (worker queue size)
}