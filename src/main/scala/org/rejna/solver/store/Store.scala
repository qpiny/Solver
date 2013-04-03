package org.rejna.solver.store

import scala.collection.JavaConversions._
import akka.actor._
import akka.pattern.ask
import akka.util.Timeout
import scala.concurrent.Future
import com.typesafe.config.Config
import com.typesafe.config.ConfigFactory
import org.rejna.solver._
import org.rejna.util.DynamicAccess
import org.rejna.solver.serializer.{ CommonTypes, SolverProtocol, SolverMessage }

trait StoreProtocol extends CommonTypes {
  SolverProtocol.registerFormat(classOf[SaveMessage], asProduct3(SaveMessage)(SaveMessage.unapply(_).get))
  SolverProtocol.registerFormat(classOf[SavedMessage], wrap[SavedMessage, Int](_.id, SavedMessage(_)))
  SolverProtocol.registerFormat(classOf[LoadMessage], asProduct2(LoadMessage)(LoadMessage.unapply(_).get))
  SolverProtocol.registerFormat(classOf[LoadedMessage], asProduct3(LoadedMessage)(LoadedMessage.unapply(_).get))
}

class StoreMessage extends SolverMessage
case class SaveMessage(requestor: Option[ActorRef], value: NodeValue, children: Array[Int]) extends StoreMessage {
  override def toString = "SaveMessage(%s, %s)".format(value, children.mkString("(", ",", ")"))
}
case class SavedMessage(id: Int) extends StoreMessage
case class LoadMessage(requestor: Option[ActorRef], id: Int) extends StoreMessage
case class LoadedMessage(id: Int, value: NodeValue, children: Array[Int]) extends StoreMessage

trait StoreCallback {
  def onSaved(id: Int) = {}
  def onLoaded(id: Int, value: NodeValue, children: Array[Int]) = {}
}

class Store(val system: ActorSystem, val config: Config) extends Extension with ClusterMember {
  lazy val storeActor = system.actorFor("/user/manager/store")
  val cluster = Cluster(system)
  cluster.subscribe(this)

  def onBecomeMaster(arefs: Iterable[ActorRef]) = {
    val nodeValueClass = Class.forName(config.getString("class"))
    val nodeValueCompanion = DynamicAccess.getCompanion[TreeCompanion[NodeValue]](nodeValueClass)
    val aref = system.actorOf(Props(new ConfigurableClassCreator[StoreActor](config.getString("store-class"), config)).withDispatcher(config.getString("dispatcher")), name = "store-actor")
    cluster.createRouter("store", IndexedSeq(aref), config.getString("dispatcher"))
  }

  def processStoreMessage(message: StoreMessage, callback: StoreCallback) = {
    message match {
      case SavedMessage(id) => callback.onSaved(id)
      case LoadedMessage(id, value, children) => callback.onLoaded(id, value, children)
    }
  }

  def save(requestor: ActorRef, value: NodeValue, children: Array[Int]): Unit =
    storeActor ! SaveMessage(Some(requestor), value, children)

  def load(requestor: ActorRef, id: Int) =
    storeActor ! LoadMessage(Some(requestor), id)

  def futureSave(value: NodeValue, children: Array[Int])(implicit timeout: Timeout) = {
    ask(storeActor, SaveMessage(None, value, children))
      .mapTo[SavedMessage]
      .map(s => (s.id))(system.dispatcher)
  }

  def futureLoad(id: Int)(implicit timeout: Timeout) =
    ask(storeActor, LoadMessage(None, id))
      .mapTo[LoadedMessage]
      .map(l => (l.id, l.value, l.children))(system.dispatcher)
}

object Store extends ExtensionId[Store] with ExtensionIdProvider with LoggingClass {
  override def lookup = Store
  override def createExtension(system: ExtendedActorSystem) = {
    log.info(s"Creation of Store extension with system: ${system}")
    val config = if (system.settings.config.hasPath("store")) {
      system.settings.config.getConfig("store")
    }
    else {
      log.info("store configuration not found")
      ConfigFactory.empty
    }
    new Store(system, config)
  }
}

abstract class StoreActor extends Actor with ActorName with LoggingClass with ConfigurableClass {
  def save(value: NodeValue, children: Array[Int]): Int
  def load(id: Int): (NodeValue, Array[Int])

  def receive = LoggingReceive(log) {
    case SaveMessage(requestor, value, children) =>
      requestor.getOrElse(sender) ! SavedMessage(save(value, children))
      PerfCounter(context.system).increment("store.save")

    case LoadMessage(requestor, id) =>
      val (value, children) = load(id)
      requestor.getOrElse(sender) ! LoadedMessage(id, value, children)
      PerfCounter(context.system).increment("store.load")
  }
}

trait StoreActorFactory {
  def getStoreActor(config: Config, maxChildren: Int): StoreActor
}
