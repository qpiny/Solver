package org.rejna.solver

import scala.collection.mutable.ListBuffer
import akka.actor._
import akka.routing._
import akka.dispatch.Dispatchers

class HashRouterActor extends Actor with ActorName with LoggingClass {
  val routee = ListBuffer[ActorRef]()

  def receive = {
    case InitRouterMessage(arefs) => routee ++= arefs
    case m @ DeadLetter(deadMessage, deadSender, deadRecipient) =>
      val aref = routee(deadMessage.hashCode.abs % routee.size)
      log.debug("Sending dead message ${m} to routee ${aref}")
      aref.forward(m)
    case m: Any =>
      val aref = routee(m.hashCode.abs % routee.size)
      log.debug("Sending message ${m} to routee ${aref}")
      aref.forward(m)
  }
}
