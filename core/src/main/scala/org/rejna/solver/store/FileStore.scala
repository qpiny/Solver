package org.rejna.solver.store

import com.typesafe.config.Config
import java.io.RandomAccessFile
import java.io.FileOutputStream
import org.rejna.solver.NodeCompute
import org.rejna.solver.TreeCompanion
import org.rejna.solver.serializer.SolverProtocol
import org.rejna.util.DynamicAccess._


class FileStoreActor(val config: Config) extends StoreActor {
  val file = new RandomAccessFile(config.getString("filename"), "rw")
  val childrenIdSize = config.getInt("children-id-size")
  val nodeValueCompanion = getCompanion[TreeCompanion[NodeCompute]](config.getString("class"))
  val entryLength = nodeValueCompanion.entryLength + nodeValueCompanion.maxChildren * childrenIdSize
  var nodeId: Int = (file.length / entryLength).asInstanceOf[Int]

  private def serializeChildren(children: Array[Int]) =
    children.flatMap(c =>
      (0 until childrenIdSize).map(i =>
        ((c >> 8 * i) & 0xff).asInstanceOf[Byte]))

  private def deserializeChildren(data: Seq[Byte]) = {
    require(data.size % childrenIdSize == 0, "Serialized children should be multiple of %d".format(childrenIdSize))
    (0 until data.size / childrenIdSize).map(c => data.slice(childrenIdSize * c, (c + 1) * childrenIdSize)).map(c => (0 /: c)((a, i) => (a << 8) + i & 0xff)).toArray
  }

  def save(nodeCompute: NodeCompute, children: Array[Int]): Int = {
    val id = nodeId
    nodeId += 1
    val data = SolverProtocol.serializeObject(nodeCompute) ++ serializeChildren(children)
    require(data.length == entryLength, "Invalid serialized data size (%d, should be %d)".format(data.length, entryLength))
    val pos = entryLength * id
    file.seek(pos)
    require(file.getFilePointer == pos, "ERROR fail to seek position " + pos)
    file.write(data)
    id
  }

  def load(id: Int): (NodeCompute, Array[Int]) = {
    val pos = entryLength * id
    file.seek(pos)
    require(file.getFilePointer == pos, "ERROR fail to seek position " + pos)
    val data = Array.ofDim[Byte](entryLength)
    file.read(data)
    (SolverProtocol.deserializeObject(data.take(entryLength), classOf[NodeCompute]),
      deserializeChildren(data.drop(entryLength)))
  }

  def size = (file.length / entryLength).asInstanceOf[Int]
}
