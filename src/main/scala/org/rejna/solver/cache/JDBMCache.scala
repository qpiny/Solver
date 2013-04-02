package org.rejna.solver.cache

import scala.collection.mutable.Map
import scala.sys.ShutdownHookThread
import java.io.{ DataOutput, DataInput }
import java.io.{ ObjectOutput, ObjectInput }
import akka.actor._
import com.typesafe.config.Config
import org.apache.jdbm.{ DB, DBMaker, Serializer }
import sbinary._
import sbinary.Operations._
import collection.JavaConversions._
import org.rejna.solver.Node
import org.rejna.solver.TreeCompanion
import org.rejna.solver.NodeValue
import org.rejna.solver.serializer.SolverProtocol
import org.rejna.util.DynamicAccess._
import org.rejna.util.DynamicAccess

class JDBMCache(val config: Config) extends NodeCacheBuilder with Serializable {
  val fileName = config.getString("file")
  val collectionName = config.getString("collection")

  object ValueSerializer extends Serializer[Either[Int, ActorRef]] with Serializable {
    import SolverProtocol.{ EitherFormat, IntFormat, ActorRefFormat, dataInputToInput, dataOutputToOutput }
    def format = EitherFormat[Int, ActorRef]
    def serialize(out: DataOutput, value: Either[Int, ActorRef]) = format.writes(out, value)
    def deserialize(in: DataInput): Either[Int, ActorRef] = {
      val oi = in.asInstanceOf[ObjectInput]
      val origAvail = oi.available
      val ret = format.reads(in)
      ret
    }
  }

  object KeySerializer extends Serializer[Node] with Serializable {
    import SolverProtocol.{ serializeObject, deserializeObject, dataInputToInput, dataOutputToOutput }
    def serialize(out: DataOutput, key: Node) = serializeObject(out, key)
    def deserialize(in: DataInput) = {
      val oi = in.asInstanceOf[ObjectInput]
      val origAvail = oi.available
      val ret = deserializeObject(in, classOf[Node])
      if (origAvail - oi.available() != 8) {
        println("Node deserializer has readed %d bytes (%d - %d)".format(origAvail - oi.available(), origAvail, oi.available()))
      }
      ret
    }
  }

  def getNodeMap(cacheId: String) = {
    val db: DB = DBMaker.openFile(fileName + cacheId)
      .disableTransactions
      .enableSoftCache
      .make
    ShutdownHookThread { db.commit }
    try {
      db.createHashMap[Node, Either[Int, ActorRef]](collectionName + cacheId, KeySerializer, ValueSerializer)
    } catch {
      case e: IllegalArgumentException =>
        val c = db.getHashMap[Node, Either[Int, ActorRef]](collectionName + cacheId)
        /* TODO remove for resume
        val i = c.iterator
        while (i.hasNext) {
          val next = i.next
          if (next._2 == null || next._2.isLeft) i.remove
        }
        */
        c
    }
  }

  override def toString = "[JDBMCache]"
}
/*
    new Map[Node, Either[Int, ActorRef]] {

      def +=(kv: (Node, Either[Int, ActorRef])) = {
        kv match {
          case (node, Left(id)) => data.put(SolverProtocol.serializeObject(node), Left(id))
          case (node, Right(worker)) => {
            val serializedWorker = akka.serialization.Serialization.currentTransportAddress.value match {
              case null => worker.path.toString()
              case address => worker.path.toStringWithAddress(address)
            }
            data.put(SolverProtocol.serializeObject(node), Right(serializedWorker))
          }
        }
        this
      }

      def -=(node: Node) = sys.error("not implemented")

      def get(node: Node) = {
        data.get(SolverProtocol.serializeObject(node)) match {
          case null => None
          case Left(id) => Some(Left(id))
          case Right(worker) => Some(Right(context.actorFor(worker)))
        }
      }

      def iterator = {
        data.map(_ match {
          case (node, Left(id)) => (SolverProtocol.deserializeObject(node, classOf[Node]), Left(id))
          case (node, Right(worker)) => (SolverProtocol.deserializeObject(node, classOf[Node]), Right(context.actorFor(worker)))
        }).iterator
      }
    }
  }

  override def toString = "[JDBMCache]"
  */

