package org.rejna.solver.serializer

import akka.actor._
import akka.serialization.Serializer
import org.rejna.solver.LoggingClass

class DebugSerializer(system: ExtendedActorSystem) extends akka.serialization.JavaSerializer(system) with LoggingClass {
  log.info("DebugSerializer initialization")

  override def fromBinary(bytes: Array[Byte], manifest: Option[Class[_]]): AnyRef = {
    val o = super.fromBinary(bytes, manifest)
    log.debug(s"Deserialize : ${o}")
    o
  }

  override def toBinary(o: AnyRef): Array[Byte] = {
    log.debug(s"Serialize : ${o}")
    super.toBinary(o)
  }

  override def identifier: Int = 33554437
}