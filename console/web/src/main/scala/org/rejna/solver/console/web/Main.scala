package org.rejna.solver.console.web

import akka.actor.ActorSystem
import akka.actor.Props

import com.typesafe.config.{ Config, ConfigFactory }

import org.mashupbots.socko.events.{ HttpResponseStatus, WebSocketHandshakeEvent }
import org.mashupbots.socko.routes._
import org.mashupbots.socko.handlers.{ StaticContentHandler, StaticContentHandlerConfig, StaticResourceRequest }
import org.mashupbots.socko.webserver.WebServer
import org.mashupbots.socko.webserver.WebServerConfig

import org.rejna.solver.DefaultSystem

object Main extends App {

  override def main(args: Array[String]) = {
    val defaultConfig = ConfigFactory.load
	val config = defaultConfig.getConfig("standalone").withFallback(defaultConfig).resolve
    DefaultSystem.setConfig(config)
    val system = DefaultSystem.system
    

    val staticHandler = system.actorOf(Props(new StaticContentHandler(new StaticContentHandlerConfig)))
    val wsHandler = system.actorOf(Props[WebSocketHandler])
    
    val routes = Routes({
      case HttpRequest(request) => request match {
        case GET(Path("/")) =>
          staticHandler ! new StaticResourceRequest(request, "www/index.html")
        case GET(PathSegments("flot" :: file :: Nil)) =>
          staticHandler ! new StaticResourceRequest(request, "www/flot/" + file)
        case GET(PathSegments("ui" :: file :: Nil)) =>
          staticHandler ! new StaticResourceRequest(request, "www/ui/" + file)
        case GET(Path(file)) =>
          staticHandler ! new StaticResourceRequest(request, "www/" + file)
        case _ => request.response.write(HttpResponseStatus.BAD_REQUEST, "Invalid request")
      }

      case WebSocketHandshake(wsHandshake) => wsHandshake match {
        case Path("/websocket/") =>
          println("Authorize websocket connection")
          wsHandshake.authorize()
      }

      case WebSocketFrame(wsFrame) => {
        println("Register websocket connection")
        wsHandler ! WSMessage(wsFrame)
      }

    })

    println("Starting Solver web console ... ")
    if (routes == null) {
      println("Routes is null")
    } else {
      val webServer = new WebServer(WebServerConfig(), routes, system)
      Runtime.getRuntime.addShutdownHook(new Thread {
        override def run { webServer.stop() }
      })
      webServer.start()

      println("Open a few browsers and navigate to http://localhost:8888/html.")
    }
  }
}

