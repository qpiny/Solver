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

  def header(title: String) = {
    val escapedtitle = scala.xml.Utility.escape(title)
    "<!DOCTYPE html>" +
      s"<html><head><title>${escapedtitle}</title></head><body><h1>${escapedtitle}</h1>"
  }

  val footer = "</body></html>"

  def s2ba(s: String) = s.map(_.toByte).toArray

  def sendFileNotFound(implicit response: HttpResponseMessage) =
    response.write(HttpResponseStatus.NOT_FOUND, s2ba(header("File not found") + footer), "text/html; charset=UTF-8", Map.empty[String, String])

  def sendInternalError(t: Throwable)(implicit response: HttpResponseMessage) = {
    val body = "<h2>" + scala.xml.Utility.escape(t.toString) + "</h2>" +
      "<pre>" + scala.xml.Utility.escape(t.getStackTraceString) + "</pre>"
    response.write(HttpResponseStatus.NOT_FOUND, s2ba(header(t.toString) + body + footer), "text/html; charset=UTF-8", Map.empty[String, String])
  }

  def sendFile(fileName: String, contentType: String)(implicit response: HttpResponseMessage) = {
    try {
      val resource = getClass.getResourceAsStream(s"/www/${fileName}")
      if (resource == null) throw new FileNotFoundException
      val source = Source.fromInputStream(resource, "UTF-8")
      try { response.write(source.mkString, contentType) }
      finally { source.close }
    } catch {
      case e: FileNotFoundException => sendFileNotFound
      case t: Throwable => sendInternalError(t)
    }
  }

  def receive = {
    case event: HttpRequestEvent =>
      if (event.request.is100ContinueExpected)
        event.response.write100Continue()
      implicit val response = event.response
      event match {
        case GET(PathSegments("html" :: fileName :: Nil)) => sendFile(fileName, "text/html; charset=UTF-8")
        case GET(Path("/")) => sendFile("index.html", "text/html; charset=UTF-8")
        case GET(PathSegments("js" :: fileName :: Nil)) => sendFile("fileName", "application/javascript")
        case _ => sendFileNotFound
      }
    case _ =>
  }
}