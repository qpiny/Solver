package org.rejna.solver

import scala.collection.mutable.HashMap
import scala.concurrent.duration._
import language.postfixOps
import java.util.concurrent.atomic.AtomicLong
import akka.actor._
import com.typesafe.config.{ Config, ConfigFactory }

class PerfCounter(system: ActorSystem, config: Config) extends Extension with LoggingClass{
  private val counters = new HashMap[String, AtomicLong]()
  private val freq = config.getInt("show-freq") milliseconds

  if (freq.toMillis > 0) {
    implicit val execCtx = system.dispatcher
    system.scheduler.schedule(2 seconds, freq) {
      log.info(toString)
    }
  }

  def increment(counterName: String) = get(counterName).incrementAndGet
  def decrement(counterName: String) = get(counterName).decrementAndGet
  def get(counterName: String) = counterName match {
    case "queue.size" => new AtomicLong(LifoBlockingQueue.size)
    case _ => counters.getOrElseUpdate(counterName, new AtomicLong(0))
  }
  def apply(counterName: String) = get(counterName).get
  override def toString = (counters + ("queue.size" -> LifoBlockingQueue.size)).toString
}

object PerfCounter extends ExtensionId[PerfCounter] with ExtensionIdProvider {
  override def lookup = PerfCounter

  override def createExtension(system: ExtendedActorSystem) = {
    val config = if (system.settings.config.hasPath("perf-counter"))
      system.settings.config.getConfig("perf-counter")
    else
      ConfigFactory.empty
    new PerfCounter(system, config)
  }
}
