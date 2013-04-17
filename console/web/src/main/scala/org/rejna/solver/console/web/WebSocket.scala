package org.rejna.solver.console.web

import scala.util.Random
import scala.collection.mutable.Set
import scala.collection.Map
import scala.collection.JavaConversions._
import scala.concurrent.duration._

import akka.actor.{ Actor, Cancellable }

import java.nio.channels.ClosedChannelException
import java.lang.management.{ ManagementFactory, OperatingSystemMXBean }
import java.util.Date

import spray.json._
import DefaultJsonProtocol._
import WebSocketProtocol._

import org.mashupbots.socko.events.WebSocketFrameEvent

import org.rejna.solver.{ Monitor, StandAlone, MonitoredValue, DefaultSystem }

case class WSMessage(frame: WebSocketFrameEvent)
case object SendData

sealed abstract class WebSocketMessage
case object StartComputation extends WebSocketMessage
case class MonitorSubscribe(filters: String*) extends WebSocketMessage
case class MonitorUnsubscribe(filters: String*) extends WebSocketMessage
case class MonitorData(timestamp: Long, counters: Map[String, Long], gauges: Map[String, MonitoredValue], system: SystemData) extends WebSocketMessage

case class SystemData(val freeMemory: Long, val totalMemory: Long, val maxMemory: Long, val cpuLoad: Double, val cpuCount: Int, val gcTime: Long) {

  def this(runtime: Runtime, osbean: OperatingSystemMXBean) = this(
    runtime.freeMemory,
    runtime.totalMemory,
    runtime.maxMemory,
    osbean.getSystemLoadAverage,
    osbean.getAvailableProcessors,
    (java.lang.management.ManagementFactory.getGarbageCollectorMXBeans :\ 0L)(_.getCollectionTime + _))

  def this() = this(Runtime.getRuntime, ManagementFactory.getOperatingSystemMXBean)
}

class WebSocketHandler extends Actor {
  val subscribers = Set.empty[WebSocketFrameEvent]
  val monitor = Monitor(DefaultSystem.system)
  var schedule: Option[Cancellable] = None

  def addSubscriber(s: WebSocketFrameEvent): Unit = {
    subscribers += s
    if (!schedule.isDefined)
      schedule = Some(context.system.scheduler.schedule(0 seconds, 1 seconds, self, SendData)(context.system.dispatcher))
  }

  def removeSubscriber(s: WebSocketFrameEvent): Unit = {
    subscribers -= s
    if (subscribers.isEmpty) {
      schedule map { _.cancel }
      schedule = None
    }
  }

  def receive = {
    case WSMessage(wsFrame) =>
      val message = wsFrame.readText
      val json = message.asJson
      json.convertTo[WebSocketMessage] match {
        case StartComputation =>
          new StandAlone().startup
        case MonitorSubscribe(filters) =>
          addSubscriber(wsFrame)
        case MonitorUnsubscribe(filters) =>
          removeSubscriber(wsFrame)
      }

    case SendData =>
      val counters = monitor.getCounters
      val gauges = monitor.getGauges

      val data = MonitorData(new Date().getTime, counters, gauges, new SystemData)
      for (s <- subscribers) {
        try {
          if (!s.channel.isConnected) {
            removeSubscriber(s)
          } else {
            s.writeText(data.toJson.compactPrint)
          }
        } catch {
          case e: ClosedChannelException => // never happens, even if we write in closed channel
            removeSubscriber(s)
        }
      }
  }
}