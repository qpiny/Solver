package org.rejna.solver

import scala.concurrent.{ Future, Await }
import scala.concurrent.duration._
import akka.actor._
import org.rejna.solver.store._
import org.rejna.solver.cache._
import org.rejna.solver.serializer.{ SolverMessage, SolverProtocol, CommonTypes }

case class ComputeMessage(requestor: ActorRef, child: Int, node: Node) extends SolverMessage
case class ResultMessage(n: Int, value: NodeValue, id: Int) extends SolverMessage

trait WorkerProtocol extends CommonTypes {
  SolverProtocol.registerFormat(classOf[ComputeMessage], asProduct3(ComputeMessage)(ComputeMessage.unapply(_).get))
  SolverProtocol.registerFormat(classOf[ResultMessage], asProduct3(ResultMessage)(ResultMessage.unapply(_).get))
}

class Worker extends Actor with LoggingClass with ActorName with CacheCallback with StoreCallback {
  val cluster = Cluster(context.system)
  val cache = Cache(context.system)
  val store = Store(context.system)
  var id = -1 // id from store
  var count = 0 // number of children actor we're waiting a result
  var requestor: ActorRef = _ // my parent
  var child: Int = -1 // child number according to my parent, [0-6[ for mancala
  var node: Node = _ // requested node computation (find node value)
  var result: NodeValue = _ // computing node value
  var children: Array[Int] = _ // my children ids
  
  override def preStart = {
    log.info(s"${this} is created")
  }

  override def onHit(_id: Int, _result: NodeValue) = {
    id = _id
    result = _result
    stop
  }

/* SYNC *************/
  override def onMiss() = {
    log.info(s"${this}: Cache miss, starting children workers")
    val nodeChildren = node.children
    children = Array.fill[Int](nodeChildren.size)(-1)
    for ((on, i) <- nodeChildren.zipWithIndex;
        n <- on) {
      cluster.enqueueActorCreation(
        self,
        self.path.elements.last + "," + i.toString(),
        classOf[Worker],
        "worker-dispatcher",
        Some(ComputeMessage(self, i, n)))
      count += 1
    }
    if (count == 0) {
      store.save(self, result, children)
    }
  }

/* ASYNC *************
  override def onMiss() = {
    //log.debug(s"${this}: Cache miss, starting children workers")
    val nodeChildren = node.children
    children = Array.fill[Int](nodeChildren.size)(-1)
    val futureChildrenRefs = nodeChildren.toList.zipWithIndex.map({ on_i =>
      on_i match {
        case (Some(n), i) =>
	        count += 1
	        Some(cluster.enqueueActorCreation(
            self,
            self.path.elements.last + "," + i.toString(),
            classOf[Worker],
            "worker-dispatcher",
            Some(ComputeMessage(self, i, n))))
                  case (None, _) => None
      }
    }).flatten
    
    if (count > 0) { // futureChildrenRefs.size > 0
      val firstCompleted = Future.firstCompletedOf(futureChildrenRefs)(context.system.dispatcher)
      Await.ready(firstCompleted, 30 seconds)
    }
    else {
      store.save(self, result, children)
    }
  }

  override def onSaved(_id: Int) = {
    id = _id
    stop
  }
*/
  def receive = LoggingReceive(log) {
    // From parent : start computation
    case ComputeMessage(_requestor, _child, _node) =>
      log.debug(s"${this} is starting computation")
      requestor = _requestor
      child = _child
      node = _node
      result = node.getValue // empty node value
      cache.checkCache(self, node)
      PerfCounter(context.system).increment("worker.start")

    // From children : partial result
    case ResultMessage(_child, _result, _id) =>
      result.update(_result)
      children(_child) = id
      count -= 1
      if (count <= 0) {
        store.save(self, result, children)
      }

    case m: CacheMessage => cache.processCacheMessage(m, this)

    case m: StoreMessage => store.processStoreMessage(m, this)

    case m: ActorRef => // return of cluster.enqueueActorCreation => ignore

    case m: Any => log.error("Unknown message :" + m)

  }

  // Computation is over
  def stop = {
    requestor ! ResultMessage(child, result, id)
    cache.cache(id, node, result, this)
    log.debug(s"Node ${this} has finished")
    PerfCounter(context.system).increment("worker.finish")
    context.stop(self)
  }
}
