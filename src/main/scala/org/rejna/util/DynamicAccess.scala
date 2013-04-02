package org.rejna.util

import scala.reflect._
import org.omg.SendingContext.RunTime

object DynamicAccess {
  def createInstanceFor[T: ClassTag](fqcn: String, args: Seq[(Class[_], AnyRef)]): T = {
    val c = getClassFor(fqcn)
    val types = args.map(_._1).toArray
    val values = args.map(_._2).toArray
    val constructor = c.getDeclaredConstructor(types: _*)
    constructor.setAccessible(true)
    val obj = constructor.newInstance(values: _*)
    val t = classTag[T].runtimeClass
    if (t.isInstance(obj))
      obj
    else
      throw new ClassCastException(fqcn + " is not a subtype of " + t)
  }

  def getClassFor[T: ClassTag](fqcn: String): Class[_ <: T] = {
    val c = getClass().getClassLoader().loadClass(fqcn).asInstanceOf[Class[_ <: T]]
    val t = classTag[T].runtimeClass
    if (t.isAssignableFrom(c))
      c
    else
      throw new ClassCastException(t + " is not assignable from " + c)
  }
  def getCompanion[T](c: Class[_]): T = getCompanion[T](c.getName)

  def getCompanion[T](c: String) = {
    val className = c match {
      case n if n.endsWith("$") => n
      case n => n + "$"
    }
    Class.forName(className).getField("MODULE$").get(null).asInstanceOf[T]
  }
}
