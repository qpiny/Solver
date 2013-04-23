package org.rejna.solver.cache

import scala.collection.JavaConversions._
import akka.actor.ActorRef
import com.typesafe.config.Config
import com.google.common.cache.CacheBuilder
import org.rejna.solver.Node
import org.rejna.solver.NodeCompute

class GuavaCache(val config: Config) extends NodeCacheBuilder with NodeComputeCacheBuilder with Serializable {
  def getNodeMap(cacheId: String) = CacheBuilder.newBuilder()
    .softValues
    .asInstanceOf[CacheBuilder[Node, Either[Int, ActorRef]]]
    .build[Node, Either[Int, ActorRef]]().asMap()

  def getNodeComputeMap(cacheId: String) = CacheBuilder.newBuilder()
    .softValues
    .asInstanceOf[CacheBuilder[Int, NodeCompute]]
    .build[Int, NodeCompute]().asMap()

  override def toString = "[GuavaCache]"
}
