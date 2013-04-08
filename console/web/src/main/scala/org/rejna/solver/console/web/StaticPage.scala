package org.rejna.solver.console.web

import scala.io.Source

import java.io.FileNotFoundException

import akka.actor.Actor

import org.mashupbots.socko.events.WebSocketHandshakeEvent
import org.mashupbots.socko.events.HttpRequestEvent
import org.mashupbots.socko.events.HttpResponseStatus

class StaticPage extends Actor {
  def receive = {
    case event: HttpRequestEvent =>
      //httpRequest.response.write(HttpResponseStatus.NOT_FOUND)
      if (event.request.is100ContinueExpected)
        event.response.write100Continue()
        try {
	      val source = Source.fromURL(getClass.getResource("www/index.html"), "UTF-8")
	      try {
	        event.response.write(source.mkString, "text/html; charset=UTF-8")
	      } finally { source.close }
        }
      catch {
        case e: FileNotFoundException =>
          event.response.write(HttpResponseStatus.NOT_FOUND)
      }
    case _ =>
  }
}

case class WSRegistration(event: WebSocketHandshakeEvent)
class WebSocketHandler extends Actor {
  def receive = {
    case _ =>
  }
}