package contege

import java.io.BufferedReader
import java.io.ByteArrayOutputStream
import java.io.FileReader
import java.io.PrintStream
import java.lang.reflect.InvocationTargetException
import java.util.ArrayList
import java.util.Date
import scala.collection.JavaConversions._
import scala.collection.mutable.Map
import scala.collection.mutable.Set
import contege.seqexec.DeadlockMonitor
import contege.seqgen.InstantiateCutTask
import contege.seqgen.Prefix
import contege.seqgen.StateChangerTask
import contege.seqgen.Suffix
import contege.seqgen.SuffixGen
import contege.seqgen.TypeManager
import javamodel.util.TypeResolver
import contege.seqexec.TestPrettyPrinter
import contege.seqexec.jpf.JPFFirstSequenceExecutor
import contege.seqexec.jpf.TSOracleJPFFirst
import java.io.File
import contege.seqexec.reflective.TSOracleNormalExec
import contege.seqexec.reflective.SequenceManager
import contege.seqexec.reflective.SequenceExecutor
import cfp.PotentialCFPs
import cfp.NextCFP
import cfp.CFPDetection
import java.util.Calendar
import contege.seqgen.TypeManager
import contege.seqexec.reflective.SequenceManager
import scala.util.control.Breaks._

class ClassTester(config: Config, stats: Stats, putClassLoader: ClassLoader, putJarPath: String, envTypes: ArrayList[String],
                  random: Random, finalizer: Finalizer, cutMethodsToTest1: Seq[MethodAtom], cutMethodsToTest2: Seq[MethodAtom], seed: Int, seedPrefix: Map[String, Prefix], typeProvider: TypeManager, cutCallsPerSeq: Int)
{
	
	private val maxSuffixLength = 10
	private val concRunRepetitions = 1
	private val nbGenTestPerNextCFP = 2
	private var maxStateChangersInPrefix = 0
	var prefixGenerated: Prefix = null
	
	
	private val seqMgr = new SequenceManager(new SequenceExecutor(stats, config), config, finalizer)
	
	private val global = new GlobalState(config, typeProvider, seqMgr, stats, random, finalizer)
	//seqMgr&global都通过config获得了cut
	
	private val tsOracle = if (config.useJPFFirst)
	{
		val jpfFirstExecutor = new JPFFirstSequenceExecutor(global, putJarPath)
		new TSOracleJPFFirst(finalizer, stats, config, seqMgr.seqExecutor, jpfFirstExecutor)
	}
	else
	{
		new TSOracleNormalExec(finalizer, concRunRepetitions, stats, seqMgr.seqExecutor, config)
	}
	
	// hack to always go through the PUT class loader -- ideally TypeResolver would be a class instead of a singleton
	TypeResolver.bcReader.classLoader = putClassLoader
	
	def run: Unit =
	{
		//又重新看run函数，可能又忘记了在哪初始化cut了
		//InstantiateCutTask(global).run产生了prefix，cut的实例化应该就在这里了，loc:ClassTester.scala.138
		//10.08 还没找到cut的实例化位置
		var nbGeneratedTests = 0L
		config.checkerListeners.foreach(l => l.updateNbGeneratedTests(nbGeneratedTests))
		
		for (suffixGenTries <- 1 to nbGenTestPerNextCFP)
		{ // generate call sequences
			//生成调用序列
			
			if (suffixGenTries == 1)
			{
				//第一次进入for循环
				maxStateChangersInPrefix = 0
				println("StateChanger:" + maxStateChangersInPrefix)
			}
			else
			{
				maxStateChangersInPrefix = 5
				println("StateChanger:" + maxStateChangersInPrefix)
			}
			
			stats.timer.start("gen")
			//生成prefix
			val prefix = getPrefix
			println("看看prefix长什么样：" + prefix);
			stats.timer.stop("gen")
			
			if (prefix == null)
			{
				return
			}
			
			stats.timer.start("gen")
			val suffixGen = new SuffixGen(prefix, maxSuffixLength, global)
			stats.timer.start("gen")
			
			stats.timer.start("gen")
			val nextSuffixOpt = suffixGen.nextSuffix(cutCallsPerSeq, cutMethodsToTest1)
			stats.timer.stop("gen")
			nextSuffixOpt match
			{
				case Some(suffix) =>
				{
					
					assert(suffix.length > 0)
					
					stats.timer.start("gen")
					val nextSuffixOpt = suffixGen.nextSuffix(cutCallsPerSeq, cutMethodsToTest2)
					stats.timer.stop("gen")
					nextSuffixOpt match
					{
						case Some(otherSuffix) =>
						{
							
							assert(otherSuffix.length > 0)
							println("Suffix 1 :" + suffix)
							println("Suffix 2 :" + otherSuffix)
							nbGeneratedTests += 1
							stats.genTests.incr
							config.checkerListeners.foreach(l => l.updateNbGeneratedTests(nbGeneratedTests))
							println("Nb generated tests: " + nbGeneratedTests)
							finalizer.currentTest = Some(TestPrettyPrinter.javaCodeFor(prefix, suffix, otherSuffix, "GeneratedTest", TestPrettyPrinter.NoOutputVectors))
							println("看看suffix、otherSuffix长什么样：" + suffix + "、" + otherSuffix);
							tsOracle.analyzeTest(prefix, suffix, otherSuffix)
							
							stats.timer.start("cfp_det")
							val cfpDetection = new CFPDetection();
							cfpDetection.detectCFP("Instrument", "Instrument_Traces");
							stats.timer.stop("cfp_det")
							
							//stats.timer.print_new(NextCFP.nextCFPMethod1 + NextCFP.nextCFPMethod2)
						}
						case None => //ignore
					}
				}
				case None => //ignore
			}
		}
		//到完全运行完中间没有return，说明没发现bug
		println("ClassTester: Could not find bug.")
	}
	
	private def getPrefix: Prefix =
	{
		if (seedPrefix.contains("" + seed + maxStateChangersInPrefix))
		{
			prefixGenerated = seedPrefix("" + seed + maxStateChangersInPrefix)
			println("Prefix:\n" + prefixGenerated)
			return prefixGenerated
		}
		else if (maxStateChangersInPrefix == 0)
		{
			new InstantiateCutTask(global).run match
			{
				//InstantiateCutTask(global).run产生了prefix，cut的实例化应该就在这里了，loc:ClassTester.scala.138
				case Some(prefix) =>
				{
					prefix.fixCutVariable
					assert(prefix.types.contains(config.cut), prefix.types)
					prefixGenerated = prefix
					seedPrefix.put("" + seed + maxStateChangersInPrefix, prefixGenerated)
					println("Prefix:\n" + prefixGenerated)
					return prefixGenerated
				}
				case None =>
				{
					config.checkerListeners.foreach(_.appendResultMsg("Cannot instantiate " + config.cut + ". Is there a public constructor or method to create it?"))
					seedPrefix.put("" + seed + maxStateChangersInPrefix, null)
					return null
				}
			}
		}
		else
		{
			prefixGenerated = appendStateChangers(seedPrefix("" + seed + 0))
			seedPrefix.put("" + seed + maxStateChangersInPrefix, prefixGenerated)
			println("Prefix:\n" + prefixGenerated)
			return prefixGenerated
		}
	}
	
	private def appendStateChangers(prefix: Prefix) =
	{
		var currentSequence = prefix
		for (_ <- 1 to maxStateChangersInPrefix)
		{
			val stateChangerTask = new StateChangerTask(currentSequence, global)
			stateChangerTask.run match
			{
				case Some(extendedSequence) =>
				{
					currentSequence = extendedSequence
				}
				case None => // ignore
			}
		}
		currentSequence
	}
}

object ClassTester extends Finalizer
{
	
	val startTime = System.currentTimeMillis
	var stats: Stats = _
	var config: Config = _
	val seedPrefix = Map[String, Prefix]()
	val seedTypeProviderMap = Map[Integer, TypeManager]()
	val seedRandomMap = Map[Integer, Random]()
	
	private val concRunRepetitions = 1
	
	def main(args: Array[String]): Unit =
	{
		println("Starting ClassTester at " + new Date())
		assert(args.size == 6 || args.size == 7)
		val cut = args(0)
		//通过参数传入了cut被测试的类
		
		val seedBase = args(2).toInt
		val maxSuffixGenTries = args(3).toInt
		val callClinit = args(5).toBoolean
		// 根据文件名读取方法名
		val selectedCUTMethods: Option[ArrayList[String]] = if (args.size == 7) Some(readMethods(args(6)))
		else None
		// isDefined 如果可选值是 Some 的实例返回 true，否则返回 false。
		if (selectedCUTMethods.isDefined) println("Focusing on " + selectedCUTMethods.get.size + " CUT methods")
		else println("No specific CUT methods selected")
		config = new Config(cut, seedBase, maxSuffixGenTries, selectedCUTMethods, new File("/tmp/"), callClinit)
		//cut作为参数创建了config
		val resultFileName = args(4)
		// 对报告文件监听，如果改变了，貌似要写日志，所以没什么重要性
		config.addCheckerListener(new ResultFileCheckerListener(resultFileName))
		
		val envTypes = new ArrayList[String]
		envTypes.add("java.lang.Object")
		// 记录含有public方法的类
		Util.addEnvTypes(args(1), envTypes, this.getClass.getClassLoader)
		
		// 打印日志，记录开机时间
		stats = new Stats
		// finalizeAndExit处停止
		stats.timer.start("all")

		// 类加载
		val typeProvider = new TypeManager(config.cut, envTypes, getClass.getClassLoader, new Random(seedBase))
		// 记住执行了哪些序列以避免重新执行它们。
		val seqMgr = new SequenceManager(new SequenceExecutor(stats, config), config, this)
		
		val global = new GlobalState(config, typeProvider, seqMgr, stats, new Random(seedBase), this)
		
		// Deadlock Detection
		// 死锁检测
		val dlMonitor = new DeadlockMonitor(config, global)
		dlMonitor.setDaemon(true)
		dlMonitor.start
		
		// Initialize Potential CFPs
		// 初始化潜在的CFP
		stats.timer.start("pot_cfp")
		// 读取方法、构造函数和类的字段
		val cutMethods = new ClassReader(Class.forName(cut, true, getClass.getClassLoader)).readMethodAtoms
		//这里的参数只是一个普通对象，并非一个在后边要使用的对象
		println("CUTMethods Size - " + cutMethods.size)
		val potentialCFPs = new PotentialCFPs();
		// 接受了一个参数列表，形成一个map映射
		potentialCFPs.writePotentialCFPs(cutMethods.mkString("@"))
		//potentialCFPs.writePotentialCFPs("java16.lang.StringBuffer.insert(int,java.lang.CharSequence)@java16.lang.StringBuffer.deleteCharAt(int)")
		stats.timer.stop("pot_cfp")
		
		var seed = seedBase;
		
		// Get next CFP from prioritizer and run test
		// 从优先级排序器中获取下一个CFP并运行测试
		val nextCFP = new NextCFP();
		//var flag = true;
		var n:Double = 0
		var f1:Double = 0
		var f2:Double = 0
		var cn:Double = 0
		var timeflag:Long = 0
		while (true)
		{
			stats.timer.start("next_cfp")
			var seed_for_next_try = nextCFP.writeNextCFP(concRunRepetitions, 2, 100);
			/*
			接下来要实现跳出的思路
			seed_for_next_try为负值，结束循环
			*/
			/*if (seed_for_next_try == -9999999)
			{
				break;
			}*/
			/*
			接下来要实现跳出的思路
			当覆盖度超过37%，结束循环
			*/
			if (cn > 0.37)
			{
				break;
			}


			seed = seed_for_next_try + seedBase;
			
			val nextCFPMethod1 = getMethodAtom(NextCFP.nextCFPMethod1, cutMethods)
			val nextCFPMethod2 = getMethodAtom(NextCFP.nextCFPMethod2, cutMethods)
			stats.timer.stop("next_cfp")
			if (nextCFPMethod1 != null && nextCFPMethod2 != null)
			{
				//var n:Double = PotentialCFPs.potCFP.size()
				n = n + 1
				if (n > 1)
				{
					f1 = NextCFP.f1
					f2 = NextCFP.f2
					cn = 1 - (f1/n) * (n-1)*f1 / ((n-1)*f1 + 2*f2)
					println("输出覆盖度Cn = " + cn)
					timeflag = (System.currentTimeMillis - startTime) / 1000
					println("经历时间（s）：" + timeflag)
				}
				val cutMethodsToTest1 = Seq(nextCFPMethod1, nextCFPMethod2)
				val cutMethodsToTest2 = Seq(nextCFPMethod2, nextCFPMethod1)
				var random: Random = null
				if (seedRandomMap.contains(seed))
				{
					random = seedRandomMap(seed)
				}
				else
				{
					random = new Random(seed)
					seedRandomMap.put(seed, random)
				}
				var typeProvider: TypeManager = null;
				if (seedTypeProviderMap.contains(seed))
				{
					typeProvider = seedTypeProviderMap(seed)
				}
				else
				{
					typeProvider = new TypeManager(config.cut, envTypes, getClass.getClassLoader, random)
					seedTypeProviderMap.put(seed, typeProvider)
				}
				
				var cutCallsPerSeq = 2
				
				if ((seed - seedBase) > 5)
				{
					cutCallsPerSeq = 5
				}
				
				val tester = new ClassTester(config, stats, getClass.getClassLoader, ".", envTypes, random, this, cutMethodsToTest1, cutMethodsToTest2, seed, seedPrefix, typeProvider, cutCallsPerSeq)
				//config作为参数构建了ClassTester的instance：tester
				println("Testing " + cut + " with seed " + seed)
				tester.run
			}
		}
		finalizeAndExit(false)
	}
	
	def finalizeAndExit(bugFound: Boolean) =
	{
		stats.timer.stop("all")
		stats.print
		stats.timer.print2
		stats.timer.print_final()
		println("Done with ClassTester at " + new Date)
		val secondsTaken = (System.currentTimeMillis - startTime) / 1000
		config.checkerListeners.foreach(_.appendResultMsg("Time (seconds): " + secondsTaken))
		
		if (bugFound)
		{
			val testCode = currentTest.get
			config.checkerListeners.foreach(_.notifyDoneAndBugFound(testCode))
		}
		else config.checkerListeners.foreach(_.notifyDoneNoBug)
		
		System.exit(0)
	}
	
	private def readMethods(fileName: String) =
	{
		val result = new ArrayList[String]()
		val r = new BufferedReader(new FileReader(fileName))
		var line = r.readLine
		while (line != null)
		{
			result.add(line)
			line = r.readLine
		}
		r.close
		result
	}
	
	private def getMethodAtom(methodName: String, cutMethods: Seq[MethodAtom]): MethodAtom =
	{
		for (m <- cutMethods)
		{
			if (m.toString.equals(methodName))
			{
				return m
			}
		}
		return null
	}
	
}
