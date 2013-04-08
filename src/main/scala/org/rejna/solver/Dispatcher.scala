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

  def getARefAndQueue(r: Runnable): (Option[ActorRef], Option[Queue[Envelope]]) = {
    val queue = try {
      // r is instance of akka.dispatch.MailBox (private)
      r.getClass.getMethod("messageQueue").invoke(r) match {
        case q: java.util.Queue[_] => Some(q.asInstanceOf[Queue[Envelope]])
        case q: Any => None
      }
    } catch { case t: Throwable => None }

    val actor = try {
      val actorCell = r.getClass.getMethod("actor").invoke(r) // .asInstanceOf[ActorCell]
      val actorRef = actorCell.getClass.getMethod("self").invoke(actorCell).asInstanceOf[ActorRef]
      Some(actorRef)
    } catch { case t: Throwable => None }

    (actor, queue)
  }
}

//          if (q.size() == 0) "0"
//          else s"${q.size} : ${q.peek.asInstanceOf[Envelope].message.getClass.getSimpleName}"

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

class WorkerQueue(val name: Option[String]) extends ConcurrentLinkedQueue[Envelope]() with QueueBasedMessageQueue with UnboundedMessageQueueSemantics {
  final def queue: Queue[Envelope] = this
}

case class SolverMailbox() extends MailboxType with LoggingClass {
  def this(settings: ActorSystem.Settings, config: Config) = this()

  final override def create(owner: Option[ActorRef], system: Option[ActorSystem]): MessageQueue = {
    val ownerName: String = owner.map(_.toString).getOrElse("NA")
    log.info(s"Creating mailbox for ${ownerName}")
    if (ownerName.indexOf("system") > 0 || ownerName == "Actor[akka://solver/]" || ownerName == "Actor[akka://solver/user]") {
      log.info(s"Monitoring mailbox ${ownerName}")
      new LoggingQueue(ownerName)
    } else {
      new ConcurrentLinkedQueue[Envelope]() with QueueBasedMessageQueue with UnboundedMessageQueueSemantics {
        final def queue: Queue[Envelope] = this
      }
    }
  }
}

class LoggingQueue(val name: String) extends ConcurrentLinkedQueue[Envelope]() with QueueBasedMessageQueue with UnboundedMessageQueueSemantics with LoggingClass {
  final def queue: Queue[Envelope] = this

  override def poll = {
    val e = super.poll //getInFifoQueue.getOrElse(super.poll(timeout, u))
    if (e != null)
      log.debug(s">> ${name} >-> ${e.message} (${e.sender})")
    e
  }

  override def remove = {
    val e = super.remove() //getInFifoQueue.getOrElse(super.poll(timeout, u))
    if (e != null)
      log.debug(s">> ${name} >-> ${e.message} (${e.sender})")
    e
  }

  override def offer(e: Envelope) = {
    log.debug(s">> ${name} <-< ${e.message} (${e.sender})");
    super.offer(e)
  }

  override def add(e: Envelope) = {
    log.debug(s">> ${name} <-< ${e.message} (${e.sender})");
    super.add(e)
  }
}

case class WorkerMailbox() extends MailboxType {

  def this(settings: ActorSystem.Settings, config: Config) = this()

  final override def create(owner: Option[ActorRef], system: Option[ActorSystem]): MessageQueue =
    new WorkerQueue(owner.map(_.path.name))
}

object WorkerComparator extends Comparator[Runnable] with LoggingClass {
  def getName(r: Runnable) = r.getClass.getMethod("messageQueue").invoke(r) match {
    case w: WorkerQueue => w.name.getOrElse { log.error("WorkerQueue has noname worker !"); "" }
    case _ => ""
  }

  def compare(r1: Runnable, r2: Runnable) = {
    val wname1 = getName(r1)
    val wlen1 = wname1.length
    val wname2 = getName(r2)
    val wlen2 = wname2.length
    if (wlen1 > PrioBlockinkQueue.currentPrio) {
      log.info(s"max prio ${PrioBlockinkQueue.currentPrio} -> ${wlen1}")
      PrioBlockinkQueue.currentPrio = wlen1
    }
    if (wlen2 > PrioBlockinkQueue.currentPrio) {
      log.info(s"max prio ${PrioBlockinkQueue.currentPrio} -> ${wlen2}")
      PrioBlockinkQueue.currentPrio = wlen2
    }

    if (wlen1 != wlen2)
      wlen2 - wlen1
    else
      wname2.compareTo(wname1)
  }
}

object PrioBlockinkQueue extends PriorityBlockingQueue[Runnable](10000, WorkerComparator) with LoggingClass {
  private def getInFifoQueue = Option(FifoBlockingQueue.poll()) //.map { x => log.info("get fifo task"); x}
  var currentPrio = 0

  override def poll(timeout: Long, u: TimeUnit) = {
    val r = super.poll(timeout, u) //getInFifoQueue.getOrElse(super.poll(timeout, u))
//    val (actor, queue) = LifoBlockingQueue.getARefAndQueue(r)
//    queue.map { q =>
//      if (q.size == 0)
//        log.info(s">> execute actor ${actor} with empty mailbox")
//    }
    r
  }

  override def offer(r: Runnable) = {
//    val (actor, queue) = LifoBlockingQueue.getARefAndQueue(r)
//    queue.map { q =>
//      if (q.size == 0)
//        log.info(s">> submit actor ${actor} with empty mailbox")
//    }
    super.offer(r)
  }
}


