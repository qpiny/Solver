package org.rejna.solver

import org.apache.log4j.PropertyConfigurator
import akka.actor._
import akka.kernel.Bootable
import com.typesafe.config.ConfigFactory
import org.rejna.solver.cache.CheckCacheMessage
import org.rejna.util.DynamicAccess

trait ActorName { me: Actor with LoggingClass =>
  override def toString = self.path.toString

  override def unhandled(message: Any) = {
    log.error("Unhandle message from ${sender} to ${this} : ${message}")
  }
}

trait LoggingClass {
  //Self: { val system: ActorSystem } =>
  //lazy val log = Logging(system, getClass.getName)
  lazy val log = org.slf4j.LoggerFactory.getLogger(this.getClass)
}

object LoggingReceive {
  def apply(log: org.slf4j.Logger)(receive: Actor.Receive) = new PartialFunction[Any, Unit] {
    def isDefinedAt(m: Any) = receive.isDefinedAt(m)
    def apply(m: Any) = {
      log.trace("Receive ${m}")
      receive(m)
    }
  }
}

class StartActor extends Actor with ActorName with LoggingClass {
  def receive = {
    case d @ DeadLetter(CheckCacheMessage(_, _), _, _) => // Ignore because process in CacheActor
    case m: Any => log.info("Receive ${m}")
  }

  override val supervisorStrategy = new SupervisorStrategy {
    import SupervisorStrategy._
    def decider = {
      case t: Any =>
        log.error(s"Fatal error : ${t.getMessage}", t)
        Stop
    }

    def handleChildTerminated(context: ActorContext, child: ActorRef, children: Iterable[ActorRef]) = {
    }

    def processFailure(context: ActorContext, restart: Boolean, child: ActorRef, cause: Throwable, stats: ChildRestartStats, children: Iterable[ChildRestartStats]) = {
    }
  }
}

class BootableBase extends Bootable with LoggingClass {
  PropertyConfigurator.configure("log4j.properties");
  val defaultConfig = ConfigFactory.load
  log.info(s"Starting Solver with profile ${this}")
  val config = defaultConfig.getConfig(getClass.getSimpleName.toLowerCase).withFallback(defaultConfig).resolve
  val system = ActorSystem("solver", config)
  val cluster = Cluster(system)

  def startup = {
    val starter = system.actorOf(Props[StartActor], name = "slave.starter")
    system.eventStream.subscribe(starter, classOf[DeadLetter])
    if (toString != "Slave") { // Standalone and Master
      cluster.init
      val rootNode = DynamicAccess.getCompanion[TreeCompanion[_]](config.getString("computation.node.class")).rootNode
      cluster.enqueueActorCreation(
        starter,
        "root",
        classOf[Worker],
        "worker-dispatcher",
        Some(ComputeMessage(starter, 0, rootNode)))
    }
  }

  def shutdown = {
    system.shutdown
  }

  override def toString = getClass.getSimpleName
}

class Slave extends BootableBase

class Master extends BootableBase

class StandAlone extends BootableBase

object Main extends App {
  override def main(args: Array[String]) = {
    if (args.isEmpty) {
      println("[error] No boot classes specified")
      System.exit(1)
    }
    val bootables = args.map(DynamicAccess.getClassFor[Bootable](_).newInstance)
    for (bootable <- bootables)
      bootable.startup
  }
}
