package org.rejna.solver.serializer

import scala.collection.mutable.ListBuffer
import scala.collection.JavaConversions._
import language.implicitConversions
import java.io.{ ByteArrayOutputStream, ByteArrayInputStream, DataInput, DataOutput }
import akka.actor._
import akka.serialization.Serializer
import scala.concurrent.{ Promise, Await }
import scala.concurrent.duration._
import akka.event.{ Logging, LogSource }
import com.typesafe.config.{ Config, ConfigFactory }
import org.rejna.solver.{ ClusterProtocol, WorkerProtocol, LoggingClass }
import org.rejna.solver.cache.CacheProtocol
import org.rejna.solver.store.StoreProtocol
import org.rejna.util.DynamicAccess
import sbinary._
import sbinary.Operations._
import sbinary.DefaultProtocol
import sbinary.DefaultProtocol.wrap
import sbinary.Input.javaInputToInput
import sbinary.Output.javaOutputToOutput

// Class derived from SolverMessage can be serializable using SolverProtocol
trait SolverMessage extends Serializable

case class SolverDeadLetter(message: SolverMessage, sender: ActorRef, recipient: ActorRef) extends SolverMessage

object SolverProtocol extends DefaultProtocol with ClusterProtocol with CacheProtocol with WorkerProtocol with StoreProtocol with LoggingClass {
  private lazy val messageTypes = ListBuffer[(Int, Summand[_ <: SolverMessage])]()
  private val systemPromise = Promise[ExtendedActorSystem]
  lazy val system = Await.result(systemPromise.future, 10 seconds)

  def setSystem(sys: ExtendedActorSystem) = systemPromise.success(sys)

  registerFormat(classOf[SolverDeadLetter], asProduct3(SolverDeadLetter)(SolverDeadLetter.unapply(_).get))

  implicit lazy val solverFormat = new Format[SolverMessage] {
    def reads(in: Input): SolverMessage = {
      val idx = read[Byte](in)
      if (idx == -1)
        null.asInstanceOf[SolverMessage]
      else
        read(in)(messageTypes(idx)._2.format)
    }

    def writes(out: Output, sm: SolverMessage): Unit = {
      if (sm == null)
        write(out, -1.asInstanceOf[Byte])
      else
        messageTypes.find(_._2.clazz.isInstance(sm)) match {
          case Some((i, sum)) =>
            write(out, i.toByte)
            write(out, sm)(sum.format.asInstanceOf[Format[SolverMessage]])
          case None =>
            messageTypes.foreach(mt => log.error("class={} format={}", mt._2.clazz, mt._2.format))
            sys.error("No known sum type for SolverMessage " + sm)
        }
    }
  }

  def registerFormat[T <: SolverMessage](clazz: Class[T], format: Format[T]) =
    messageTypes += messageTypes.size -> Summand(clazz, format)

  private def writeSum[T](out: Output, sm: SolverMessage, sum: Summand[T]) = {
    write(out, sum.clazz.cast(sm).asInstanceOf[T])(sum.format)
  }

  def serializeObject(out: Output, o: SolverMessage) = {
    messageTypes.find(_._2.clazz.isInstance(o)) match {
      case Some((i, sum)) => writeSum(out, o, sum)
      case None =>
        messageTypes.foreach(mt => log.error("class={} format={}", mt._2.clazz, mt._2.format))
        sys.error("No known sum type for object " + o)
    }
  }
  def serializeObject(o: SolverMessage): Array[Byte] = {
    val out = new ByteArrayOutputStream
    serializeObject(out, o)
    out.toByteArray
  }

  def deserializeObject[T <: SolverMessage](in: Array[Byte], clazz: Class[_ >: T]): T =
    deserializeObject(new ByteArrayInputStream(in), clazz)

  def deserializeObject[T <: SolverMessage](in: Input, clazz: Class[_ >: T]): T = {
    messageTypes.find(s => clazz.isAssignableFrom(s._2.clazz)) match {
      case Some((i, sum)) => read(in)(sum.format).asInstanceOf[T]
      case None => {
        log.error(s"messageTypes=${messageTypes}")
        sys.error(s"No known sum type for clazz ${clazz}")
      }
    }
  }

  implicit def dataInputToInput(in: DataInput) = new Input {
    def readByte: Byte = in.readByte()
    override def readFully(target: Array[Byte], offset: Int, length: Int) = in.readFully(target, offset, length)
    override def readFully(target: Array[Byte]) = in.readFully(target)
  }

  implicit def dataOutputToOutput(out: DataOutput) = new Output {
    def writeByte(value: Byte) = out.writeByte(value)
    override def writeAll(source: Array[Byte], offset: Int, length: Int) = out.write(source, offset, length)
    override def writeAll(source: Array[Byte]) = out.write(source)
  }

}

class SBSerializer(val system: ExtendedActorSystem) extends Serializer with LoggingClass {
  log.debug("SBSerializer initialization")
  SolverProtocol.setSystem(system)

  def fromBinary(bytes: Array[Byte], manifest: Option[Class[_]]): AnyRef = {
    SolverProtocol.solverFormat.reads(new ByteArrayInputStream(bytes)) match {
      case SolverDeadLetter(m, s, r) => DeadLetter(m, s, r)
      case m: AnyRef => m
    }
  }

  def toBinary(o: AnyRef): Array[Byte] = {
    val bao = new ByteArrayOutputStream
    o match {
      case sm: SolverMessage => SolverProtocol.solverFormat.writes(bao, sm)
      case dl @ DeadLetter(m: SolverMessage, s, r) =>
        SolverProtocol.solverFormat.writes(bao, SolverDeadLetter(m, s, r))
    }

    bao.toByteArray
  }

  def identifier: Int = 33554436

  def includeManifest = false
}
