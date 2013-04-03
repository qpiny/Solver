package org.rejna.solver.serializer

import scala.collection.JavaConversions._
import scala.concurrent.duration._
import language.postfixOps
import akka.actor.{ ActorRef, ExtendedActorSystem }
import com.typesafe.config.{ Config, ConfigFactory }
import sbinary._
import sbinary.DefaultProtocol._
import sbinary.Operations._
import org.rejna.solver.{ Node, NodeValue, ConfigurableClass, ConfigurableClassCreator }
import org.rejna.util.DynamicAccess

trait CommonTypes extends DefaultProtocol {
  val system: ExtendedActorSystem

  implicit val actorRefFormat = wrap[ActorRef, String](aref =>
    akka.serialization.Serialization.currentTransportAddress.value match {
      case null => aref.path.toString()
      case address => aref.path.toStringWithAddress(address)
    }, system.actorFor(_))

  implicit val durationFormat = wrap[Duration, Long](_.toMillis, _ milliseconds)

  implicit val configFormat = wrap[Config, scala.collection.immutable.Map[String, String]](
    _.root.unwrapped.map { case (k, v) ⇒ (k, v.toString) } toMap,
    ConfigFactory.parseMap(_))

  implicit def configurableClassFormat[T <: ConfigurableClass] = new Format[T] {
    def reads(in: Input) =
      DynamicAccess.createInstanceFor[ConfigurableClass](
        read[String](in),
        Seq((classOf[Config], read[Config](in)))).asInstanceOf[T]
    def writes(out: Output, confClass: T) = {
      write[String](out, confClass.getClass.getName)
      write[Config](out, confClass.config)
    }
  }

  implicit val configurableClassCreatorFormat = new Format[ConfigurableClassCreator[AnyRef]] {
    def reads(in: Input) =
      DynamicAccess.createInstanceFor(
        read[String](in),
        Seq((classOf[Config], read[Config](in))))
    def writes(out: Output, confClass: ConfigurableClassCreator[AnyRef]) = {
      write[String](out, confClass.className)
      write[Config](out, confClass.config)
    }
  }
  SolverProtocol.registerFormat(classOf[ConfigurableClassCreator[AnyRef]], configurableClassCreatorFormat)

  def solverMessageFormat[T <: SolverMessage](clazz: Class[_ <: T]) = new Format[T] {
    def reads(in: Input) = SolverProtocol.deserializeObject(in, clazz)
    def writes(out: Output, obj: T) = SolverProtocol.serializeObject(out, obj)
  }

  def eitherFormat[T1, T2](implicit bin1: Format[T1], bin2: Format[T2]) = new Format[Either[T1, T2]] {
    def reads(in: Input) = read[Byte](in) match {
      case 0 => Left(read[T1](in))
      case 1 => Right(read[T2](in))
      case b => sys.error("Invalid serialized data for either (first byte = %d)".format(b))
    }

    def writes(out: Output, e: Either[T1, T2]) =
      e match {
        case Left(t1) =>
          write[Byte](out, 0)
          write[T1](out, t1)
        case Right(t2) =>
          write[Byte](out, 1)
          write[T2](out, t2)
      }
  }

  implicit val nodeValueFormat: Format[NodeValue] = solverMessageFormat[NodeValue](classOf[NodeValue])

  implicit val nodeFormat: Format[Node] = solverMessageFormat[Node](classOf[Node])
}