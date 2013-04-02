package org.rejna.solver

import java.util.concurrent.{ TimeUnit, ThreadFactory, ExecutorService, LinkedBlockingDeque }
import scala.concurrent.duration._
import akka.event.LoggingAdapter
import akka.actor.{ ActorSystem, ActorRef, ActorContext }
import akka.dispatch._
import akka.dispatch.Dispatcher
import akka.dispatch.MonitorableThreadFactory
import com.typesafe.config.Config

class LifoThreadPoolExecutor(config: Config, prerequisites: DispatcherPrerequisites) extends ExecutorServiceConfigurator(config, prerequisites) {
  import ThreadPoolConfigBuilder.conf_?

  val threadPoolConfig: ThreadPoolConfig = createThreadPoolConfigBuilder(config, prerequisites).config

  protected def createThreadPoolConfigBuilder(config: Config, prerequisites: DispatcherPrerequisites): ThreadPoolConfigBuilder = {
    ThreadPoolConfigBuilder(ThreadPoolConfig())
      .setKeepAliveTime(Duration(config getMilliseconds "keep-alive-time", TimeUnit.MILLISECONDS))
      .setAllowCoreThreadTimeout(config getBoolean "allow-core-timeout")
      .setCorePoolSizeFromFactor(config getInt "core-pool-size-min", config getDouble "core-pool-size-factor", config getInt "core-pool-size-max")
      .setMaxPoolSizeFromFactor(config getInt "max-pool-size-min", config getDouble "max-pool-size-factor", config getInt "max-pool-size-max")
      .withNewThreadPoolWithCustomBlockingQueue(LifoBlockingQueue)
  }

  def createExecutorServiceFactory(id: String, threadFactory: ThreadFactory): ExecutorServiceFactory = {
    val tf = threadFactory match {
      case m: MonitorableThreadFactory ⇒
        // add the dispatcher id to the thread names
        m.copy(m.name + "-" + id)
      case other ⇒ other
    }
    threadPoolConfig.createExecutorServiceFactory(id, tf)
  }
}

// Contains worker runnable queue
// it acts as stack (last submitted work is executed first)
import java.util.Collection
object LifoBlockingQueue extends LinkedBlockingDeque[Runnable] {
  private var logger: Option[LoggingAdapter] = None
  def setLogger(log: LoggingAdapter) = logger = Some(log)

  override def offer(r: Runnable) = { logger.map(_.info("addThread {} (size={})", r.hashCode, size)); offerFirst(r) }
  override def offer(r: Runnable, timeout: Long, u: TimeUnit) = { logger.map(_.info("XXX offer2 (size={}", size)); offerFirst(r, timeout, u) }
  override def add(r: Runnable) = { logger.map(_.info("XXX add (size={}", size)); offerFirst(r) }
  override def put(r: Runnable) = { logger.map(_.info("XXX put (size={}", size)); putFirst(r) }
  override def poll(timeout: Long, u: TimeUnit) = {
    val r = super.poll(timeout, u)
    logger.map(_.info("removeThread {} (size={})", r.hashCode, size))
    r
  }
  override def take = { logger.map(_.info("XXX take (size={}", size)); super.take }
  override def remove(a: Any) = { logger.map(_.info("XXX remove (size={}", size)); super.remove(a) }
  override def drainTo(c: Collection[_ >: Runnable]) = { logger.map(_.info("XXX drainTo (size={}", size)); super.drainTo(c) }
  override def drainTo(c: Collection[_ >: Runnable], m: Int) = { logger.map(_.info("XXX drainTo2 (size={}", size)); super.drainTo(c, m) }
  
}
