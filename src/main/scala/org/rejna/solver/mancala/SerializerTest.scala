import sbinary._
import sbinary.DefaultProtocol._
import sbinary.Operations._

import org.rejna.solver._
import org.rejna.solver.mancala._

object SerializerTest {
	def doIt = {
	  val g = new Game()
	  g.do_play(3)
	  g.do_play(4)
	  println(g)
	  val data = toByteArray(g)(Game.GameFormat)
	  println(data.mkString("(", ",", ")"))
	  val g2 = fromByteArray(data)(Game.GameFormat)
	  println(g2)
	  println(g2 == g)
	}
	
	def gameStatTest = {
	  val data1 = Array.ofDim[Byte](GameStat.entryLength)
	  scala.util.Random.nextBytes(data1)
	  println(data1.mkString("(", ",", ")"))
	  val gs = fromByteArray(data1)(GameStat.GameStatFormat)
	  println(gs)
	  val data2 = toByteArray(gs)(GameStat.GameStatFormat)
	  println(data2.mkString("(", ",", ")"))
	  println(data1.deep == data2.deep)
	}
}

//SerializerTest.gameStatTest