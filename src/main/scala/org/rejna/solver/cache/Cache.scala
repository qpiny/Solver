package org.rejna.solver.cache

import scala.collection.mutable.Map
import scala.collection.mutable.ListBuffer
import scala.collection.JavaConversions._
import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import language.postfixOps
import akka.actor._
import akka.remote.RemoteScope
import akka.event._
import akka.dispatch._
import akka.serialization.SerializationExtension
import com.typesafe.config.Config
import com.typesafe.config.ConfigFactory
import sbinary._
import sbinary.DefaultProtocol._
import sbinary.Operations._
import org.rejna.solver._
import org.rejna.solver.store._
import org.rejna.util.DynamicAccess
import org.rejna.solver.serializer.SolverMessage
import org.rejna.solver.serializer.{ CommonTypes, SolverProtocol, SolverMessage }

abstract class CacheMessage extends SolverMessage
case class InitCacheMessage(nodesBuilder: NodeCacheBuilder, valuesBuilder: ValueCacheBuilder, cacheId: String) extends CacheMessage
case class CheckCacheMessage(requestor: ActorRef, node: Node) extends CacheMessage { override def hashCode: Int = node.hashCode }
case class CacheMissMessage() extends CacheMessage
case class CacheHitMessage(id: Int, value: NodeValue) extends CacheMessage
case class CacheDataMessage(id: Int, node: Node, value: NodeValue) extends CacheMessage { override def hashCode: Int = node.hashCode }

trait CacheProtocol extends CommonTypes {
  SolverProtocol.registerFormat(classOf[InitCacheMessage], asProduct3(InitCacheMessage)(InitCacheMessage.unapply(_).get))
  SolverProtocol.registerFormat(classOf[CheckCacheMessage], asProduct2(CheckCacheMessage)(CheckCacheMessage.unapply(_).get))
  SolverProtocol.registerFormat(classOf[CacheMissMessage], asSingleton(CacheMissMessage()))
  SolverProtocol.registerFormat(classOf[CacheHitMessage], asProduct2(CacheHitMessage)(CacheHitMessage.unapply(_).get))
  SolverProtocol.registerFormat(classOf[CacheDataMessage], asProduct3(CacheDataMessage)(CacheDataMessage.unapply(_).get))
}

trait CacheCallback {
  val waiting = new ListBuffer[ActorRef]

  def onHit(id: Int, value: NodeValue): Unit = {}

  def onMiss: Unit = {}

  def addWaiter(requestor: ActorRef): Unit = {
    waiting += requestor
  }
}

class Cache(val system: ActorSystem, val config: Config) extends Extension with ClusterMember with LoggingClass {
  lazy val cacheActor = {
    val aref = system.actorFor("/user/manager/cache")
    system.eventStream.subscribe(aref, classOf[DeadLetter])
    aref
  }
  val cluster = Cluster(system)
  cluster.subscribe(this)
  //  implicit def ec = ExecutionContext.defaultExecutionContext(system)

  def onBecomeMaster(arefs: Iterable[ActorRef]): Unit = {
    log.debug("onBecomeMaster: start")
    val nodesBuilder = DynamicAccess.createInstanceFor[NodeCacheBuilder](
      config.getString("node.cache-class"),
      Seq((classOf[Config], config.getConfig("node"))))

    val valuesBuilder = DynamicAccess.createInstanceFor[ValueCacheBuilder](
      config.getString("value.cache-class"),
      Seq((classOf[Config], config.getConfig("node"))))

    val cacheFutureActors = (0 until config.getInt("number-of-cache")).map(i =>
      cluster.createClusteredActor(
          "cache_" + i,
          classOf[CacheActor],
          config.getString("dispatcher"),
          Some(InitCacheMessage(nodesBuilder, valuesBuilder, i.toString))))

    val cacheActors = Await.result(Future.sequence(cacheFutureActors).map(_.flatten), 5 seconds)
    Await.ready(cluster.createRouter("cache", cacheActors, config.getString("dispatcher")), 5 seconds)
  }

  def checkCache(requestor: ActorRef, node: Node): Unit = {
    log.debug("checkCache: node=%s from=%s to=%s".format(node, requestor, cacheActor))
    cacheActor.tell(CheckCacheMessage(requestor, node), requestor)
  }

  def cache(id: Int, node: Node, value: NodeValue, callback: CacheCallback): Unit = {
    cacheActor ! CacheDataMessage(id, node, value) // we lost original sender but we don't care, we don't expect any return message
    for (w <- callback.waiting)
      w ! CacheHitMessage(id, value)
  }

  def processCacheMessage(message: CacheMessage, callback: CacheCallback): Unit = {
    message match {
      case m @ CacheHitMessage(id, value) => callback.onHit(id, value)
      case CacheMissMessage() => callback.onMiss
      case CheckCacheMessage(requestor, node) => callback.addWaiter(requestor)
    }
  }
}

object Cache extends ExtensionId[Cache] with ExtensionIdProvider {
  override def lookup: ExtensionId[Cache] = Cache
  override def createExtension(system: ExtendedActorSystem): Cache = {
    val config = if (system.settings.config.hasPath("cache")) {
      system.settings.config.getConfig("cache")
    }
    else {
      ConfigFactory.empty
    }
    new Cache(system, config)
  }
}

class CacheActor extends Actor with ActorName with ActorLogging {
  private val store = Store(context.system)
  private val promiseNodes = Promise[Map[Node, Either[Int, ActorRef]]]
  private lazy val nodes = Await.result(promiseNodes.future, 10 seconds)
  private val promiseValues = Promise[Map[Int, NodeValue]]
  private lazy val values = Await.result(promiseValues.future, 10 seconds)
  //var nodes: Option[Map[Node, Either[Int, ActorRef]]] = None
  //var values: Option[Map[Int, NodeValue]] = None

  log.debug("CacheActor: instantiation actorRef=%s".format(self))

  def receive = {
    case InitCacheMessage(nodesBuilder, valuesBuilder, cacheId) =>
      log.debug("CacheActor >> InitCache: cacheId=%s".format(cacheId))
      promiseNodes.success(nodesBuilder.getNodeMap(cacheId))
      promiseValues.success(valuesBuilder.getValueMap(cacheId))
    //nodes = Some(nodesBuilder.getNodeMap(cacheId))
    //values = Some(valuesBuilder.getValueMap(cacheId))

    case m @ CheckCacheMessage(requestor, node) =>
      log.debug("CacheActor >> CheckCacheMessagecache: node=%s".format(node))
      checkCache(requestor, node)

    case m @ CacheDataMessage(id, node, value) =>
      nodes += node -> Left(id)
      values += id -> value

    case m: StoreMessage =>
      store.processStoreMessage(m, new StoreCallback {})

    case DeadLetter(CheckCacheMessage(requestor, node), sender, recipient) =>
      self.tell(CheckCacheMessage(requestor, node), requestor)

    case dl: DeadLetter => log.error("CacheActor >> DeadLetter : " + dl)
  }

  def checkCache(requestor: ActorRef, node: Node) = {
    nodes.get(node) match {
      case None => // not in cache
        nodes.update(node, Right(requestor))
        requestor ! CacheMissMessage()
        PerfCounter(context.system).increment("cache.miss")

      case Some(Right(worker)) => // node is being computed by worker
        worker ! CheckCacheMessage(requestor, node)
        PerfCounter(context.system).increment("cache.forward")

      case Some(Left(id)) => // node id is in cache now check value cache
        values.get(id) match {
          case None => // value is not in cache, load it from store
            store.futureLoad(id)(10 seconds).onSuccess { // XXX adjust timeout
              case (id, value, children) => requestor ! CacheHitMessage(id, value)
            }
          case Some(value) => // value is in cache
            requestor ! CacheHitMessage(id, value)
        }
        PerfCounter(context.system).increment("cache.hit")
    }
  }
}

trait NodeCacheBuilder extends ConfigurableClass {
  def getNodeMap(cacheId: String): Map[Node, Either[Int, ActorRef]]
}

trait ValueCacheBuilder extends ConfigurableClass {
  def getValueMap(cacheId: String): Map[Int, NodeValue]
}
