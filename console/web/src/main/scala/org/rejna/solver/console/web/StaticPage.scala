package org.rejna.solver.console.web

import scala.io.Source

import java.io.FileNotFoundException

import akka.actor.Actor

import org.mashupbots.socko.routes._
import org.mashupbots.socko.events.WebSocketHandshakeEvent
import org.mashupbots.socko.events.HttpRequestEvent
import org.mashupbots.socko.events.HttpResponseStatus
import org.mashupbots.socko.events.HttpResponseMessage

class StaticPage extends Actor {
  
  def sendFileNotFound(response: HttpResponseMessage) = response.write(HttpResponseStatus.NOT_FOUND, """<!DOCTYPE html>
      <html>
      <head><title>File not found</title></head>
      <body><h1>File not found</h1></body>
      </html>""");
  
  def sendInternalError(response: HttpResponseMessage, t: Throwable) = {
    val message = new StringBuilder("""<!DOCTYPE html>
<html>
<head><title>Internal server error</title></head>
<body><h1>Internal server error</h1><pre>""")
        scala.xml.Utility.escape(t.getStackTraceString, message)
        message ++= "</pre></body></html>"
        response.write(HttpResponseStatus.INTERNAL_SERVER_ERROR, message.toString)
  } 
  
  def sendFile(fileName: String, response: HttpResponseMessage) = {
    try {
      val source = Source.fromURL(getClass.getResource(s"www/${fileName}"), "UTF-8")
      try {
        response.write(source.mkString, "text/html; charset=UTF-8")
      } finally { source.close }
    } catch {
      //        case e: FileNotFoundException => sendFileNotFound(response)
      case t: Throwable => sendInternalError(response, t)
    }
  }
  
  
  def receive = {
    case event: HttpRequestEvent =>
      //httpRequest.response.write(HttpResponseStatus.NOT_FOUND)
      if (event.request.is100ContinueExpected)
        event.response.write100Continue()
      event match {
        case GET(PathSegments("html" :: fileName :: Nil)) => sendFile(fileName, event.response)
        case GET(Path("/")) => sendFile("index.html", event.response)
        case _ => sendFileNotFound(event.response)
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