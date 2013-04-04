package org.rejna.solver

import java.util.concurrent.{ TimeUnit, ThreadFactory, ExecutorService, LinkedBlockingDeque }
import scala.concurrent.duration._
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
object LifoBlockingQueue extends LinkedBlockingDeque[Runnable] with LoggingClass {
  override def offer(r: Runnable) = { log.debug(s">> (${size}) + ${showRunnable(r)} "); offerFirst(r) }
  override def offer(r: Runnable, timeout: Long, u: TimeUnit) = { log.debug("XXX offer2 (size=${size}"); offerFirst(r, timeout, u) }
  override def add(r: Runnable) = { log.debug("XXX add (size=${size}"); offerFirst(r) }
  override def put(r: Runnable) = { log.debug("XXX put (size=${size}"); putFirst(r) }
  override def poll(timeout: Long, u: TimeUnit) = {
    val r = super.poll(timeout, u)
    log.debug(s">> (${size}) - ${showRunnable(r)}")
    r
  }
  override def take = { log.debug("XXX take (size=${size}"); super.take }
  override def remove(a: Any) = { log.debug("XXX remove (size=${size}"); super.remove(a) }
  override def drainTo(c: Collection[_ >: Runnable]) = { log.debug("XXX drainTo (size=${size}"); super.drainTo(c) }
  override def drainTo(c: Collection[_ >: Runnable], m: Int) = { log.debug("XXX drainTo2 (size=${size}"); super.drainTo(c, m) }
  def showRunnable(r: Runnable): String = {
    val queue = try {
      // r is instance of akka.dispatch.MailBox (private)
      r.getClass.getMethod("messageQueue").invoke(r) match {
        case q if q == LifoBlockingQueue => "TODO : LifoBlockingQueue.peek"
        case q: Any => "NotLifoBlockingQueue"
      }
    }
    catch { case t: Throwable => "NA" }
    
    val actor = try {
    	val actorCell = r.getClass.getMethod("actor").invoke(r)
    	val actorRef = actorCell.getClass.getMethod("self").invoke(actorCell)
    	actorRef.toString }
    catch { case t: Throwable => "NA" }
    
    //s"class ${r.getClass} numberOfMessages=${numberOfMessages} actor=${actor}"
    s"${actor}[${queue}]"
  }
}
