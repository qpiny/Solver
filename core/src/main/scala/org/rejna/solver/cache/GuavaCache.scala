package org.rejna.solver.cache

import scala.collection.JavaConversions._
import akka.actor.ActorRef
import com.typesafe.config.Config
import com.google.common.cache.CacheBuilder
import org.rejna.solver.Node
import org.rejna.solver.NodeValue

class GuavaCache(val config: Config) extends NodeCacheBuilder with ValueCacheBuilder with Serializable {
  def getNodeMap(cacheId: String) = CacheBuilder.newBuilder()
    .softValues
    .asInstanceOf[CacheBuilder[Node, Either[Int, ActorRef]]]
    .build[Node, Either[Int, ActorRef]]().asMap()

  def getValueMap(cacheId: String) = CacheBuilder.newBuilder()
    .softValues
    .asInstanceOf[CacheBuilder[Int, NodeValue]]
    .build[Int, NodeValue]().asMap()

  override def toString = "[GuavaCache]"
}
