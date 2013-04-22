package org.rejna.solver

import java.util.concurrent.{ TimeUnit, ThreadFactory, ExecutorService, LinkedBlockingDeque, LinkedBlockingQueue, BlockingQueue, PriorityBlockingQueue, ConcurrentLinkedQueue }
import java.util.{ Comparator, Queue }

import scala.concurrent.duration._
import scala.collection.mutable.{ SynchronizedQueue, SynchronizedStack }

import akka.actor.{ ActorSystem, ActorRef, ActorContext }
import akka.dispatch._
import akka.dispatch.Dispatcher
import akka.dispatch.MonitorableThreadFactory

import com.typesafe.config.Config

class MonitoredThreadPoolExecutor(config: Config, prerequisites: DispatcherPrerequisites) extends ExecutorServiceConfigurator(config, prerequisites) {
  //import ThreadPoolConfigBuilder.conf_?

  def createExecutorServiceFactory(id: String, threadFactory: ThreadFactory): ExecutorServiceFactory = {
    ThreadPoolConfigBuilder(ThreadPoolConfig())
      .setKeepAliveTime(Duration(config getMilliseconds "keep-alive-time", TimeUnit.MILLISECONDS))
      .setAllowCoreThreadTimeout(config getBoolean "allow-core-timeout")
      .setCorePoolSizeFromFactor(config getInt "core-pool-size-min", config getDouble "core-pool-size-factor", config getInt "core-pool-size-max")
      .setMaxPoolSizeFromFactor(config getInt "max-pool-size-min", config getDouble "max-pool-size-factor", config getInt "max-pool-size-max")
      .withNewThreadPoolWithCustomBlockingQueue(config getString "queue-type" match {
        case "fifo" => new MonitoredThreadQueue(id, new LinkedBlockingQueue[Runnable])
        case "lifo" => new MonitoredThreadQueue(id, new LifoBlockingQueue)
        case "prio" => new MonitoredThreadQueue(id, new PrioBlockinkQueue)
        case "mprio" => new MonitoredThreadQueue(id, new MonitoredPrioBlockingQueue(id))
        case _ => new MonitoredThreadQueue(id, new LinkedBlockingQueue[Runnable]) // default is fifo queue
      })
      .config.createExecutorServiceFactory(id, threadFactory)
  }
}

class LifoBlockingQueue extends LinkedBlockingDeque[Runnable] with LoggingClass {
  override def add(r: Runnable) = { addFirst(r); true }
  override def offer(r: Runnable) = offerFirst(r)
  override def put(r: Runnable) = putFirst(r)
  override def offer(r: Runnable, timeout: Long, unit: TimeUnit) = offerFirst(r, timeout, unit)
}

case class MonitoredMailbox(monitored: Boolean) extends MailboxType {
  
  import collection.JavaConversions._
  def this(settings: ActorSystem.Settings, config: Config) = {
    this(config.hasPath("monitored") && config.getBoolean("monitored"))
  }

  final override def create(owner: Option[ActorRef], system: Option[ActorSystem]): MessageQueue = {
    val ownerName = owner.map(_.path.name).getOrElse("UnknownActor")
    val q = new ConcurrentLinkedQueue[Envelope]() with QueueBasedMessageQueue with UnboundedMessageQueueSemantics with NamedMailQueue {
      val name = ownerName
      final def queue: Queue[Envelope] = this
    }
    if (!monitored || ownerName.startsWith("root") || ownerName.startsWith("$")) q // worker and temp mailbox mustn't be monitored
    else new MonitoredMailQueue(ownerName, q)
  }

}

object WorkerComparator extends Comparator[Runnable] with LoggingClass {
  def getName(r: Runnable) = r.getClass.getMethod("messageQueue").invoke(r) match {
    case nmq: NamedMailQueue => nmq.name
    case _ => "UnmonitoredActor"
  }

  def compare(r1: Runnable, r2: Runnable) = {
    val wname1 = getName(r1)
    val wlen1 = wname1.length
    val wname2 = getName(r2)
    val wlen2 = wname2.length

    if (wlen1 != wlen2)
      wlen2 - wlen1
    else
      wname2.compareTo(wname1)
  }
}

class PrioBlockinkQueue extends PriorityBlockingQueue[Runnable](10000, WorkerComparator)
