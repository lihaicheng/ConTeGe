package contege

import scala.collection.JavaConversions._
import scala.collection.mutable.Set
import scala.collection.mutable.Map
import java.util.ArrayList
import java.io.PrintStream
import java.io.File
import contege.seqgen.TypedParameter

class Config(val cut: String, // class under test,cut是用val修饰的所以在类体之外是可见的
		            val seed: Int, val maxSuffixGenTries: Int,
		            val selectedCUTMethodsForSuffix: Option[ArrayList[String]],
		            val workingDir: File,
		            val callClinit: Boolean) {
	
	def maxPrefixes = {
		var result = maxSuffixGenTries / 20 // try to create 20 suffixes for each prefix
		if (result < 1) result = 1
		result
	}
	
	val shareOnlyCUTObject = false
	
	val useJPFFirst = false
	
	//Nil是一个空的List，定义为List[Nothing]
	private var checkerListenersVar: List[CheckerListener] = Nil
	def addCheckerListener(l: CheckerListener) = {
		//:: 该方法被称为cons，意为构造，向队列的头部追加数据，创造新的列表。用法为 x::list,其中x为加入到头部的元素，无论x是列表与否，它都只将成为新生成列表的第一个元素，也就是说新生成的列表长度为list的长度＋1(btw, x::list等价于list.::(x))
	  checkerListenersVar = l :: checkerListenersVar
	}
	def checkerListeners = checkerListenersVar
}

class SubclassTesterConfig(cut: String,  // the subclass
                        val oracleClass: String,   // the class to use as an oracle, e.g. the superclass
                        val constructorMap: ConstructorMapping, // map from superclass constructors to semantically equivalent subclass constructors; superclass constructors w/o a mapping cannot be used to be create CUT instances
		                seed: Int,
		                maxSuffixGenTries: Int,
		                val maxTests: Int,
		                val concurrentMode: Boolean,
		                selectedCUTMethods: Option[ArrayList[String]],
		                workingDir: File,
		                val compareOutputVectors: Boolean,
		                val stopOnOutputDiff: Boolean) extends Config(cut, seed, maxSuffixGenTries, selectedCUTMethods, workingDir, true) {
}

class PathTesterConfig (cut: String, val targetCutMethod: String, val targetMethodParameters: Set[List[Option[TypedParameter]]], seed: Int, val maxTests: Int, workingDir: File) extends Config(cut, seed, 0, None, workingDir, false) {
    
}