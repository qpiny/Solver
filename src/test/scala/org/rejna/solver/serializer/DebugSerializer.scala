package org.rejna.solver.serializer

import akka.actor._
import akka.serialization.Serializer
import org.rejna.solver.LoggingClass

class DebugSerializer(val system: ExtendedActorSystem) extends Serializer with LoggingClass {
  log.info("DebugSerializer initialization")

  def fromBinary(bytes: Array[Byte], manifest: Option[Class[_]]): AnyRef =
   sys.error(s"debug deserialization is not implemented")

  def toBinary(o: AnyRef): Array[Byte] = {
    log.error("Message: ${o}")
    Array[Byte]()
  }

  def identifier: Int = 33554437

  def includeManifest = false
}