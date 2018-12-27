package contege

import scala.collection.JavaConversions._
import java.util.{Random => JRandom}
import java.util.ArrayList

// makes the whole process deterministic to allow replay when the random seed is known
// 使整个过程具有确定性，随机种子已知时允许重播
class Random(val seed: Int) {

	// 由于seed的存在，结果每次过程都是相同的
	private var rand = new JRandom(seed)
	
	def nextInt(excludedMax: Int) = rand.nextInt(excludedMax)
	
	def nextInt = rand.nextInt
	
	def nextBool = rand.nextBoolean
	
	def nextByte = {
		val bytes = new Array[Byte](1)
		rand.nextBytes(bytes)
		bytes(0)
	}
		
	def nextShort = {
		val max = java.lang.Short.MAX_VALUE
		rand.nextInt(max*2 + 2) - max - 2
	}
	
	def nextLong = rand.nextLong
	
	def nextChar = rand.nextInt.asInstanceOf[Char]
	
	def nextFloat = rand.nextFloat
	
	def nextDouble = rand.nextDouble
	
	def chooseOne[T](list: Seq[T]) = {
		list.get(nextInt(list.size))
	}
	
	def chooseOne[T](set: Set[T]) = {
	    set.toList.get(nextInt(set.size))
	}
	
}
