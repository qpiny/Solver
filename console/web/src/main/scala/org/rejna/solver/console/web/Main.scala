package org.rejna.solver.console.web

import akka.actor.ActorSystem
import akka.actor.Props

import org.mashupbots.socko.events.HttpResponseStatus
import org.mashupbots.socko.events.WebSocketHandshakeEvent
import org.mashupbots.socko.routes._
import org.mashupbots.socko.webserver.WebServer
import org.mashupbots.socko.webserver.WebServerConfig

object Main extends App {

  override def main(args: Array[String]) = {
    val actorSystem = ActorSystem("StandAloneConsole")
    val wsHandler = actorSystem.actorOf(Props[WebSocketHandler], "webSocketBroadcaster")
    val routes = Routes({
      case HttpRequest(httpRequest) => httpRequest match {
          
      case GET(_) => actorSystem.actorOf(Props[StaticPage]) ! httpRequest
        //case Path("/favicon.ico") => httpRequest.response.write(HttpResponseStatus.NOT_FOUND)
      }

      case WebSocketHandshake(wsHandshake) => wsHandshake match {
        case Path("/websocket/") =>
          wsHandshake.authorize()
      }

      case WebSocketFrame(wsFrame) => wsHandler ! wsFrame

    })

    println("Starting Solver web console ... ")
    if (routes == null) {
      println("Routes is null")
    } else {
      val webServer = new WebServer(WebServerConfig(), routes, actorSystem)
      Runtime.getRuntime.addShutdownHook(new Thread {
        override def run { webServer.stop() }
      })
      webServer.start()

      println("Open a few browsers and navigate to http://localhost:8888/html.")
    }
  }
}

