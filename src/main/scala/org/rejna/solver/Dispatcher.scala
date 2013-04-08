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

class SolverThreadPoolExecutor(config: Config, prerequisites: DispatcherPrerequisites) extends ExecutorServiceConfigurator(config, prerequisites) with LoggingClass {
  import ThreadPoolConfigBuilder.conf_?

  val threadPoolConfig: ThreadPoolConfig = createThreadPoolConfigBuilder(config, prerequisites).config

  protected def createThreadPoolConfigBuilder(config: Config, prerequisites: DispatcherPrerequisites): ThreadPoolConfigBuilder = {
    ThreadPoolConfigBuilder(ThreadPoolConfig())
      .setKeepAliveTime(Duration(config getMilliseconds "keep-alive-time", TimeUnit.MILLISECONDS))
      .setAllowCoreThreadTimeout(config getBoolean "allow-core-timeout")
      .setCorePoolSizeFromFactor(config getInt "core-pool-size-min", config getDouble "core-pool-size-factor", config getInt "core-pool-size-max")
      .setMaxPoolSizeFromFactor(config getInt "max-pool-size-min", config getDouble "max-pool-size-factor", config getInt "max-pool-size-max")
      .withNewThreadPoolWithCustomBlockingQueue(config getString "queue-type" match {
        case "fifo" => { log.info("Using FifoBlovkingQueue"); FifoBlockingQueue }
        case "lifo" => { log.info("Using FiloBlockingQueue"); LifoBlockingQueue }
        case "prio" => { log.info("Using PrioBlockinkQueue"); PrioBlockinkQueue }
        case _ => sys.error("Invalid queue type (should be fifo, lifo or prio)")
      })
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
// it acts as stack (first submitted work is executed first)
import java.util.Collection

object LifoBlockingQueue extends LinkedBlockingDeque[Runnable] with LoggingClass {

  private def getInFifoQueue = Option(FifoBlockingQueue.poll())

  override def offer(r: Runnable) = offerFirst(r) //{ log.debug(s">> (${size}) + ${showRunnable(r)}"); super.offer(r) }

  override def poll(timeout: Long, u: TimeUnit) = {
    val r = getInFifoQueue.getOrElse(super.poll(timeout, u))
    //log.debug(s">> (${size}) - ${showRunnable(r)}")
    r
  }
  override def poll = getInFifoQueue.getOrElse(pollFirst)

  def showRunnable(r: Runnable): String = {
    val queue = try {
      // r is instance of akka.dispatch.MailBox (private)
      r.getClass.getMethod("messageQueue").invoke(r) match {
        case q: java.util.Queue[_] =>
          if (q.size() == 0) "0"
          else s"${q.size} : ${q.peek.asInstanceOf[Envelope].message.getClass.getSimpleName}"
        case q: Any => s"not a Queue : ${q.getClass.toString}"
      }
    } catch { case t: Throwable => "NA" }

    val actor = try {
      val actorCell = r.getClass.getMethod("actor").invoke(r)
      val actorRef = actorCell.getClass.getMethod("self").invoke(actorCell)
      actorRef.toString
    } catch { case t: Throwable => "NA" }

    s"${actor}[${queue}]"
  }
}

// Contains worker runnable queue
// it acts as queue (last submitted work is executed first)
object FifoBlockingQueue extends LinkedBlockingQueue[Runnable] with LoggingClass {

  //  override def poll(timeout: Long, u: TimeUnit) = {
  //    val r = super.poll(timeout, u)
  //     log.debug(s">> (${size}) - ${LifoBlockingQueue.showRunnable(r)}")
  //    r
  //  }
  //  
  //  override def offer(r: Runnable) = { log.debug(s">> (${size}) + ${LifoBlockingQueue.showRunnable(r)}"); super.offer(r) }
}

class WorkerQueue(val prio: Option[Int]) extends ConcurrentLinkedQueue[Envelope]() with QueueBasedMessageQueue with UnboundedMessageQueueSemantics {
  final def queue: Queue[Envelope] = this
}

case class WorkerMailbox() extends MailboxType {

  def this(settings: ActorSystem.Settings, config: Config) = this()

  final override def create(owner: Option[ActorRef], system: Option[ActorSystem]): MessageQueue =
    new WorkerQueue(owner.map(_.path.name.length))
}

object WorkerComparator extends Comparator[Runnable] with LoggingClass {
  def getPrio(r: Runnable) = r.getClass.getMethod("messageQueue").invoke(r) match {
    case w: WorkerQueue => w.prio.getOrElse(-1)
    case _ => -1
  }

  def compare(r1: Runnable, r2: Runnable) = {
    val prio1 = getPrio(r1)
    val prio2 = getPrio(r2)
    if (prio1 > PrioBlockinkQueue.currentPrio) {
      log.info(s"max prio ${PrioBlockinkQueue.currentPrio} -> ${prio1}")
      PrioBlockinkQueue.currentPrio = prio1
    }
    if (prio2 > PrioBlockinkQueue.currentPrio) {
      log.info(s"max prio ${PrioBlockinkQueue.currentPrio} -> ${prio2}")
      PrioBlockinkQueue.currentPrio = prio2
    }
      
    prio2 - prio1 // FIXME check sign
  }
}

object PrioBlockinkQueue extends PriorityBlockingQueue[Runnable](10000, WorkerComparator) with LoggingClass {
  private def getInFifoQueue = Option(FifoBlockingQueue.poll()) //.map { x => log.info("get fifo task"); x}
  var currentPrio = 0

  override def poll(timeout: Long, u: TimeUnit) = {
    val r = super.poll(timeout, u) //getInFifoQueue.getOrElse(super.poll(timeout, u))
    log.debug(s">> (${size}) - ${LifoBlockingQueue.showRunnable(r)}")
    r
  }

  override def offer(r: Runnable) = { log.debug(s">> (${size}) + ${LifoBlockingQueue.showRunnable(r)}"); super.offer(r) }
}


