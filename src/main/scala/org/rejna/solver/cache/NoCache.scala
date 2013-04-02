package org.rejna.solver.cache

import scala.collection.mutable.Map
import akka.actor._
import com.typesafe.config.Config
import org.rejna.solver.Node
import org.rejna.solver.NodeValue

class NoCache(val config: Config) extends NodeCacheBuilder {

  def getNodeMap(cacheId: String) = new Map[Node, Either[Int, ActorRef]] {
    def -=(node: Node) = this
    def +=(kv: (Node, Either[Int, ActorRef])) = this
    def iterator = sys.error("not implemented")
    def get(node: Node) = None
  }
}
