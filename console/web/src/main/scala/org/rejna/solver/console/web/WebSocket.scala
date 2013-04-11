package org.rejna.solver.console.web

import scala.util.Random
import scala.collection.mutable.Set
import scala.concurrent.duration._

import akka.actor.Actor

import java.nio.channels.ClosedChannelException
import java.lang.management.ManagementFactory
import java.util.Date

import spray.json._
import DefaultJsonProtocol._
import WebSocketProtocol._

import org.mashupbots.socko.events.WebSocketFrameEvent

import org.rejna.solver.{ PerfCounter, StandAlone }

case class WSRegistration(frame: WebSocketFrameEvent)
case object SendData

sealed abstract class WebSocketMessage
case object StartComputation extends WebSocketMessage
case class MonitorSubscribe(filters: String*) extends WebSocketMessage
case class MonitorUnsubscribe(filters: String*) extends WebSocketMessage


class WebSocketHandler extends Actor {
  val subscribers = Set.empty[WebSocketFrameEvent]

  override def preStart = {
    context.system.scheduler.schedule(2 seconds, 1 seconds, self, SendData)(context.system.dispatcher)
  }
  
  def getCpuLoad = ManagementFactory.getOperatingSystemMXBean.getSystemLoadAverage
  def receive = {
    case WSRegistration(wsFrame) =>
      val json = wsFrame.readText.asJson
      json.convertTo[WebSocketMessage] match {
        case StartComputation => new StandAlone().startup
        case MonitorSubscribe(filters) => subscribers += wsFrame
        case MonitorUnsubscribe(filters) => subscribers -= wsFrame
      }
      
      println("client registered")
    case SendData =>
      lazy val cpuLoad = getCpuLoad
      for (s <- subscribers) {
        try {
          if (!s.channel.isConnected) {
            subscribers -= s
          } else {
            s.writeText(JsObject(Map(
              "type" -> JsString("data"),
              "data" -> JsObject(Map(
                "timestamp" -> JsString(new Date().getTime.toString),
                "load" -> JsString(cpuLoad.toString),
                "random1" -> JsString((Random.nextInt & 0xff).toString),
                "random2" -> JsString((Random.nextInt & 0xff).toString))))).compactPrint)
          }
        } catch {
          case e: ClosedChannelException => // never happens, even if we write in closed channel
            subscribers -= s
        }
      }
  }
}