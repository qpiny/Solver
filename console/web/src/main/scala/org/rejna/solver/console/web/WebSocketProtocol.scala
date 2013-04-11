package org.rejna.solver.console.web

import spray.json._
import DefaultJsonProtocol._

object WebSocketProtocol extends DefaultJsonProtocol {
  implicit object WebSocketFormat extends RootJsonFormat[WebSocketMessage] {
    def write(m: WebSocketMessage) = m match {
      case StartComputation =>
        JsObject(Map("format" -> JsString("StartComputation")))
      case MonitorSubscribe(filters @ _*) =>
        JsObject(
          "format" -> JsString("MonitorSubscribe"),
          "data" -> JsArray(filters.map(JsString(_)): _*))
      case MonitorUnsubscribe(filters @ _*) =>
        JsObject(
          "format" -> JsString("MonitorUnsubscribe"),
          "data" -> JsArray(filters.map(JsString(_)): _*))
    }

    def toStrList(values: List[JsValue]) = values map {
      _ match {
        case JsString(s) => s
        case _ => deserializationError("Filter must be a string")
      }
    }

    def read(value: JsValue) = value.asJsObject.getFields("format", "data") match {
      case Seq(JsString(format)) => format match {
        case "StartComputation" => StartComputation
        case _ => deserializationError("Invalid WebSocketMessage format")
      }
      case Seq(JsString(format), JsArray(data)) => format match {
        case "MonitorSubscribe" => MonitorSubscribe(toStrList(data): _*)
        case "MonitorUnsubscribe" => MonitorUnsubscribe(toStrList(data): _*)
        case _ => deserializationError("Invalid WebSocketMessage format")
      }
    }
  }
}