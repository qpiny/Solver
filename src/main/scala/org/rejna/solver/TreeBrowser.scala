package org.rejna.solver

import akka.actor._
import akka.dispatch._
import akka.pattern.ask

import org.rejna.util.StringColumn._
/*
trait TreeBrowser extends Store with ComputationTree {
  val store: ActorRef
  def systemContext: ActorSystem
  implicit val ec = systemContext.dispatcher
  val rootNode: Node

  case object Start

  class Browser extends Actor {
    var currentNode = rootNode

    def receive = {
      case Start =>
        currentNode = rootNode
        store ! GetLastId
      case Id(id) =>
        store ! Load(id)
      case Loaded(id, value, children) =>
        val x = Await.result(Future.sequence(
          children // Array[node id]
            .zipWithIndex // Array[(node id, index)]
            .filter(ci => {
              println("index=" + ci._2 + " nodeId=" + ci._1)
              ci._1 != -1})
            .map(ci => ask(store, Load(ci._1))(500 millis) // Array[Future[Loaded.value + index]]
              .mapTo[Loaded]
              .map(x => x.value.toString + "\n" + ci._2))
            .toList), 1 second)
        println("Node id = " + id)
        println(currentNode)
        println("###########################################")
        println(value)
        println("###########################################")
        println(makeColumn(x: _*))
        /*
        for ((c, i) <- children.zipWithIndex) {
          if (c != -1) {
            choiceCount += 1
            val f = ask(store, Load(c))(500 millis))
            print("[" + f + "]")
          }
        }
        */
        println("\nYour choice :")
        var choice = 0
        var nextNode: Option[Node] = None
        do {
          choice = Console.readInt
          nextNode = currentNode.children.collectFirst { case x if x._1 == choice => x._2 }
        } while (nextNode.isEmpty)
        store ! Load(children(choice))
        currentNode = nextNode.get

    }
  }

  def start = {
    systemContext.actorOf(Props(new Browser), name = "Browser") ! Start
  }
}*/ 