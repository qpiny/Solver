package org.rejna.solver

import scala.reflect.ClassTag
import com.typesafe.config.Config
import org.rejna.util.DynamicAccess
import org.rejna.solver.serializer.SolverMessage

trait TreeCompanion[T] {
  def maxChildren: Int
  def rootNode: Node
  val entryLength: Int
  def registerSerializer
}

trait HasTreeCompanion

trait Node extends HasTreeCompanion with SolverMessage {
  def children: Array[Option[Node]] // to be replaced by Iterable[(Int, Node)] ?
  def getNodeCompute: NodeCompute
}

trait NodeCompute extends HasTreeCompanion with SolverMessage {
  def getNodeValue(n: Node): NodeValue
  def update(nc: NodeCompute): NodeCompute
}

trait NodeValue

trait ConfigurableClass extends SolverMessage {
  val config: Config
}

class ConfigurableClassCreator[+T: ClassTag](val className: String, val config: Config) extends Function0[T] with SolverMessage with Serializable {
  def apply() = {
    DynamicAccess.createInstanceFor[T](
      className,
      Seq((classOf[Config] -> config)))
  }
}