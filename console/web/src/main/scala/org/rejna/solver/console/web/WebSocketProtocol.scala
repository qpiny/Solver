package org.rejna.solver.console.web

import scala.collection.Map

import spray.json._
import DefaultJsonProtocol.jsonFormat3

import org.rejna.solver.MonitoredValue

object WebSocketProtocol extends DefaultJsonProtocol {
  implicit object WebSocketFormat extends RootJsonReader[WebSocketMessage] {
    
    def toStrList(values: List[JsValue]) = values map {
      _ match {
        case JsString(s) => s
        case _ => deserializationError("Filter must be a string")
      }
    }

    def read(value: JsValue) = value.asJsObject.getFields("type", "data") match {
      case Seq(JsString(jtype)) => jtype match {
        case "StartComputation" => StartComputation
        case _ => deserializationError("Invalid WebSocketMessage format")
      }
      case Seq(JsString(jtype), JsArray(data)) => jtype match {
        case "MonitorSubscribe" => MonitorSubscribe(toStrList(data): _*)
        case "MonitorUnsubscribe" => MonitorUnsubscribe(toStrList(data): _*)
        case _ => deserializationError("Invalid WebSocketMessage format")
      }
    }
  }

  implicit object CounterFormat extends JsonWriter[Map[String, Long]] {
    def write(m: Map[String, Long]) = JsObject(scala.collection.immutable.Map(m.mapValues(JsNumber(_)).toList: _*))
  }

  implicit object MonitoredValueFormat extends JsonWriter[MonitoredValue] {
    def write(v: MonitoredValue) = JsObject(
      "min" -> JsNumber(v.min),
      "current" -> JsNumber(v.current),
      "max" -> JsNumber(v.max))
  }

  implicit object VariableFormat extends JsonWriter[Map[String, MonitoredValue]] {
    def write(m: Map[String, MonitoredValue]) = JsObject(scala.collection.immutable.Map(m.mapValues(_.toJson).toList: _*))
  }

  implicit object MonitorDataFormat extends JsonWriter[MonitorData] {
    def write(d: MonitorData) = JsObject(
      "type" -> JsString("MonitorData"),
      "data" -> JsObject(
        "timestamp" -> JsNumber(d.timestamp),
        "counters" -> d.counters.toJson,
        "variables" -> d.gauges.toJson,
        "memory" -> d.memory.toJson))
  }
}