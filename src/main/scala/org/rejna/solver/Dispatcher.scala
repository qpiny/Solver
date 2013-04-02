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
object LifoBlockingQueue extends LinkedBlockingDeque[Runnable] {
	override def offer(r: Runnable) = { println("offer : " + size); offerFirst(r) }
	override def offer(r: Runnable, timeout: Long, u: TimeUnit) = { println("offer w/ timeout : " + size); offerFirst(r, timeout, u) }
	override def add(r: Runnable) = { println("add : " + size); offerFirst(r) }
	override def put(r: Runnable) = { println("put : " + size); putFirst(r) }
	override def remove = { println("remove : " + size); super.remove }
	override def poll = { println("poll : " + size); super.poll }
	override def take = { println("take : " + size); super.take }
	//override def poll(timeout: Long, u: TimeUnit) = { println("poll w/ timeout : " + size); super.poll(timeout, u) }
}
