package org.rejna.solver.cache

import scala.collection.JavaConversions._
import akka.actor.ActorRef
import akka.actor.ActorRefFactory
import akka.actor.ActorContext
import com.typesafe.config.Config
import com.google.common.cache.CacheBuilder
import org.rejna.solver.Node
import org.rejna.solver.NodeValue

/*
class GuavaCacheFactory extends NodeCacheFactory with ValueCacheFactory {
  def initNodeCache(config: Config, serializer: Serializer[Node]): NodeCacheBuilder =
    new GuavaNodeCache(config, serializer)

  def initValueCache(config: Config, serializer: Serializer[NodeValue]): ValueCacheBuilder =
    new GuavaValueCache(config, serializer)

  override def toString = "[GuavaCacheFactory]"
}

*/
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
