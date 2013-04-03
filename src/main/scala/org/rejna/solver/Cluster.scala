package org.rejna.solver

import scala.util.Random
import scala.math.max
import scala.concurrent._
import scala.concurrent.duration._
import scala.collection.mutable.{ ListBuffer, HashMap }
import scala.collection.JavaConversions._
import scala.language.postfixOps
import scala.language.existentials
import akka.actor._
import akka.remote._
import akka.pattern.ask
import akka.serialization._
import java.util.concurrent.locks.ReentrantReadWriteLock
import com.typesafe.config.{ Config, ConfigFactory }

import org.rejna.solver.serializer.{ SolverProtocol, SolverMessage, CommonTypes }
import org.rejna.util.DynamicAccess

/* Message definitions */
object InitManagerMessage {
  def apply(clusterMembers: Iterable[ActorRef], freq: Duration, nodeClass: String, valueClass: String) = // use Class instead of String
    new InitManagerMessage(clusterMembers.toArray, freq.toMillis, nodeClass, valueClass)
  def unapply(im: InitManagerMessage) = Some((im.clusterMembers, im.freq milliseconds, im.nodeClass, im.valueClass))
}
class InitManagerMessage(val clusterMembers: Array[ActorRef], val freq: Long, val nodeClass: String, val valueClass: String) extends SolverMessage {
  val membersString = clusterMembers.mkString("(", ",", ")")
  override def toString = s"InitManagerMessage(clusterMemebers=${membersString}, freq=${freq}, nodeClass=${nodeClass}, valueClass=${valueClass}"
}

case class SendQueueSizeMessage() extends SolverMessage
case class QueueSizeMessage(managerRef: ActorRef, size: Int) extends SolverMessage
case class CreateActorMessage(name: String, actorClass: Class[_ <: Actor], dispatcher: String, message: Option[SolverMessage]) extends SolverMessage
case class InitRouterMessage(arefs: Array[ActorRef]) extends SolverMessage {
  val arefsString = arefs.mkString("(", ",", ")")
  override def toString = s"InitRouterMessage(${arefsString})"
}

/* Message serialization */
trait ClusterProtocol extends CommonTypes {
  import SolverProtocol.{ solverFormat, registerFormat }

  registerFormat(classOf[InitManagerMessage], asProduct4((m: Array[ActorRef], f: Duration, nodeClass: String, valueClass: String) =>
    new InitManagerMessage(m, f.toMillis, nodeClass, valueClass))(InitManagerMessage.unapply(_).get))
  registerFormat(classOf[SendQueueSizeMessage], asSingleton(SendQueueSizeMessage()))
  registerFormat(classOf[QueueSizeMessage], asProduct2(QueueSizeMessage)(QueueSizeMessage.unapply(_).get))
  registerFormat(classOf[CreateActorMessage], asProduct4((n: String, c: Class[_], d: String, m: Option[SolverMessage]) =>
    CreateActorMessage(n, c.asInstanceOf[Class[Actor]], d, m))(CreateActorMessage.unapply(_).get))
  registerFormat(classOf[InitRouterMessage], wrap[InitRouterMessage, Array[ActorRef]](_.arefs, InitRouterMessage))
}

trait ClusterMember {
  def onBecomeMaster(clusterMembers: Iterable[ActorRef])
}

// This actor is the manager of a node (member of a cluster)
class Manager extends Actor with LoggingClass with ActorName {
  val cluster = Cluster(context.system)
  val perf = PerfCounter(context.system)
  implicit val execCtx = context.system.dispatcher

  override val supervisorStrategy = new SupervisorStrategy {
    import SupervisorStrategy._
    def decider = {
      case t: Throwable =>
        log.error(s"Fatal error : ${t.getMessage}", t)
        Stop
    }
    def handleChildTerminated(context: ActorContext, child: ActorRef, children: Iterable[ActorRef]) = {}
    def processFailure(context: ActorContext, restart: Boolean, child: ActorRef,
      cause: Throwable, stats: ChildRestartStats, children: Iterable[ChildRestartStats]) = {}
  }

  def receive = LoggingReceive(log) {
    case InitManagerMessage(clusterMembers, freq, nodeClass, valueClass) =>
      cluster.registerManagers(self, clusterMembers)
      DynamicAccess.getCompanion[TreeCompanion[_]](nodeClass).registerSerializer
      DynamicAccess.getCompanion[TreeCompanion[_]](valueClass).registerSerializer
      if (freq.length > 0) context.system.scheduler.schedule(2 seconds, freq, self, SendQueueSizeMessage())

    case SendQueueSizeMessage() => Future {
      val queueSizeMessage = QueueSizeMessage(self, perf("queue.size").toInt) // + pc("worker.start").toInt - pc("worker.finish").toInt)
      cluster.managers.keys.foreach(aref => aref ! queueSizeMessage)
    }

    case QueueSizeMessage(managerRef, size) =>
      Future {
        cluster.updateManagerQueueSize(managerRef, size)
      }

    case CreateActorMessage(name, actorClass, dispatcher, message) => {
      val requestor = sender
      Future {
        val aref = context.actorOf(Props(actorClass).withDispatcher(dispatcher), name)
        message.map(aref.tell(_, sender))
        requestor ! aref
      }
    }
  }
}

class Cluster(val system: ActorSystem, val config: Config) extends Extension with LoggingClass {
  implicit val execCtx = system.dispatcher
  
  class Remote(val actorRef: ActorRef, val isLocal: Boolean) {
    var queueSize = 0
    val deploy = Deploy(scope = RemoteScope(actorRef.path.address))
  }

  private val initialized = Promise[Unit]()
  private val _managers = new HashMap[ActorRef, Remote]
  private val localManager = system.actorOf(Props[Manager], name = "manager")
  private var _isMaster = false
  private val members = ListBuffer[ClusterMember]()
  private val membersLock = new ReentrantReadWriteLock()
  private val (syncFreq, managerRefs) = try { // get both or none
    (config.getMilliseconds("sync-frequency").toLong milliseconds,
      List(config.getStringList("slaves"): _*).map(uri => system.actorFor(uri)) :+ localManager)
  } catch {
    case e: Exception =>
      (0L milliseconds, List[ActorRef](localManager))
  }

  def waitInitialization = if (!initialized.isCompleted) Await.ready(initialized.future, 10 seconds)

  def isMaster = _isMaster

  def init = {
    log.info("initCluster: managerRefs={}", managerRefs)
    managerRefs.foreach(aref => {
      val message = InitManagerMessage(managerRefs.filterNot(_ == aref), syncFreq, config.getString("node.class"), config.getString("value.class"))
      aref ! message
    }) // XXX need to wait end of managers initialization ?
    _isMaster = true

    membersLock.readLock.lock
    try { for (m <- members) m.onBecomeMaster(managerRefs) }
    finally { membersLock.readLock.unlock }
  }

  def subscribe(member: ClusterMember) = {
    membersLock.writeLock.lock
    try {
      members += member
      if (isMaster) member.onBecomeMaster(managerRefs)
    } finally { membersLock.writeLock.unlock }
  }

  def registerManagers(localManager: ActorRef, remoteManagers: Array[ActorRef]) = {
    if (initialized.isCompleted) {
      sys.error("Cluster is alread initialized")
    }
    _managers += localManager -> new Remote(localManager, true)
    _managers ++= remoteManagers.map(rm => rm -> new Remote(rm, false))
    initialized.success(Unit)
  }

  def managers = {
    waitInitialization
    _managers.toMap
  }

  def updateManagerQueueSize(managerRef: ActorRef, queueSize: Int) = {
    waitInitialization
    _managers(managerRef).queueSize = queueSize
  }

  private def getRemote(sender: ActorRef): Remote = {
    waitInitialization
    var acc: Double = 0
    val avg = if (_managers.isEmpty) 0 else _managers.values.map(_.queueSize).sum / _managers.size
    val threshold = Random.nextDouble
    val localSize = _managers(sender).queueSize
    // SUM(MAX(size - avg, 0)) == SUM(MAX(avg - size, 0))
    val deltaSum = _managers.values.map(v => max(v.queueSize - avg, 0.0)).sum
    val coeff = max(localSize - avg, 0.0) / deltaSum
    if (0 < coeff) {
      for ((a, r) <- _managers) {
        acc += coeff * max(avg - r.queueSize, 0.0) / deltaSum
        if (acc > threshold) {
          return r
        }
      }
    }
    _managers(localManager)
  }

  private def getDeploy(sender: ActorRef): Deploy = getRemote(sender).deploy

  // create actor in each node (ask manager to create actor locally)
  def createClusteredActor(name: String, actorClass: Class[_ <: Actor], dispatcher: String, message: Option[SolverMessage] = None): Future[Iterable[ActorRef]] = {
    waitInitialization
    log.debug("createClusteredActor: name=${name} actorClass=${actorClass} message=${message}")
    val arefFutureList = _managers.keys.map(aref => {
      ask(aref, CreateActorMessage(name, actorClass, dispatcher, message))(10 seconds).mapTo[ActorRef]
    })
    Future.sequence(arefFutureList)
  }

  // create a router in each node with list of worker actors
  def createRouter(name: String, arefs: IndexedSeq[ActorRef], dispatcher: String): Future[Iterable[ActorRef]] = {
    waitInitialization
    log.debug("createRouter: name=${name} arefs=${arefs}")
    val arefFutureList = _managers.keys.map(aref =>
      ask(aref, CreateActorMessage(name, classOf[HashRouterActor], dispatcher, Some(InitRouterMessage(arefs.toArray))))(10 seconds).mapTo[ActorRef]
    )
    Future.sequence(arefFutureList)
  }

  // ask actor creation in node with lower load
  def enqueueActorCreation(requester: ActorRef, name: String, actorClass: Class[_ <: Actor], dispatcher: String, message: Option[SolverMessage] = None): Unit = {
    waitInitialization
    val remote = getRemote(localManager)
    if (remote.isLocal) {
      log.debug("enqueueActorCreation(local): name=${name}")
      val aref = system.actorOf(Props(actorClass).withDispatcher(dispatcher), name)
      message.map(aref.tell(_, requester))
      requester ! aref
    } else {
      val target = remote.actorRef
      log.debug("enqueueActorCreation(${target}): name=${name}")
      target.tell(CreateActorMessage(name, actorClass, dispatcher, message), requester)
    }
  }
}

object Cluster extends ExtensionId[Cluster] with ExtensionIdProvider {
  override def lookup = Cluster
  override def createExtension(system: ExtendedActorSystem) = {
    val config = if (system.settings.config.hasPath("cluster")) {
      system.settings.config.getConfig("cluster")
    } else {
      ConfigFactory.empty
    }
    new Cluster(system, config)
  }
}
