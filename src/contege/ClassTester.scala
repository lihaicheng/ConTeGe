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
import scala.util.control.Exception.allCatch

class ClassTester(config: Config, stats: Stats, putClassLoader: ClassLoader, putJarPath: String, envTypes: ArrayList[String],
    random: Random, finalizer: Finalizer) {

    // PLDI 2012
    private val cutCallsPerSeq = 2
    private val maxSuffixLength = 10
    private val concRunRepetitions = 100
    private val maxStateChangersInPrefix = 5

//    private val cutCallsPerSeq = 2
//    private val maxSuffixLength = 10
//    private val concRunRepetitions = 1000
//    private val maxStateChangersInPrefix = 5


    // kept separately to ensure deterministic random selection of one
    // 分开保存，保证随机选择的确定性
    private val prefixes = new ArrayList[Prefix] 
    private val prefix2SuffixGen = Map[Prefix, SuffixGen]()
    private val prefix2Suffixes = Map[Prefix, ArrayList[Suffix]]()

    // envTypes 包含java.lang.Object和env_types.txt里的类名，还有cut，也就是被测试类
    private val typeProvider = new TypeManager(config.cut, envTypes, putClassLoader, random)

    private val seqMgr = new SequenceManager(new SequenceExecutor(stats, config), config, finalizer)

    private val global = new GlobalState(config, typeProvider, seqMgr, stats, random, finalizer)

    private val tsOracle = if (config.useJPFFirst) {
        val jpfFirstExecutor = new JPFFirstSequenceExecutor(global, putJarPath)
        new TSOracleJPFFirst(finalizer, stats, config, seqMgr.seqExecutor, jpfFirstExecutor)
    } else {
        new TSOracleNormalExec(finalizer, concRunRepetitions, stats, seqMgr.seqExecutor, config)
    }

    // hack to always go through the PUT class loader -- ideally TypeResolver would be a class instead of a singleton 
    TypeResolver.bcReader.classLoader = putClassLoader

    def run: Unit = {
        val dlMonitor = new DeadlockMonitor(config, global)
        dlMonitor.setDaemon(true)
        dlMonitor.start

        var nbGeneratedTests = 0L
        // 全部更新成0？
        // updateNbGeneratedTests是一个空函数，也就是什么也没做
        // 这里想要干啥？？？
        config.checkerListeners.foreach(l => l.updateNbGeneratedTests(nbGeneratedTests))

        // 如果maxSuffixGenTries的值到现在没有改变，那么现在还是从参数传入的值
        for (suffixGenTries <- 1 to config.maxSuffixGenTries) { // generate call sequences
            println("打个点，看看执行到哪了")
            println("顺便看看suffixGenTries和config.maxSuffixGenTries变不变" + suffixGenTries + "、" + config.maxSuffixGenTries)
            stats.timer.start("gen")
            // 前缀生成
            val prefix = getPrefix
            println("打个点，看看执行到哪了，是否经过new prefix")
            stats.timer.stop("gen")

            // 后缀生成类，用来生成后缀
            val suffixGen = prefix2SuffixGen(prefix)
            // 后缀集合，现在刚创建应当为空
            val suffixes = prefix2Suffixes(prefix)

            stats.timer.start("gen")
            // cutCallsPerSeq值为2，为什么是2？
            val nextSuffixOpt = suffixGen.nextSuffix(cutCallsPerSeq)
            stats.timer.stop("gen")
            
            nextSuffixOpt match {
                case Some(suffix) => {
                    assert(suffix.length > 0)
                    if (!suffixes.exists(oldSuffix => suffix.equivalentTo(oldSuffix))) {
                        suffixes += suffix
                        println("New suffix \n" + suffix)
                        // run the new sequences against all existing sequences and again itself
                        // 对所有现有序列运行新序列，并再次对其本身运行
                        suffixes.foreach(otherSuffix => {
                            nbGeneratedTests += 1
                            config.checkerListeners.foreach(l => l.updateNbGeneratedTests(nbGeneratedTests))
                            println("Nb generated tests: " + nbGeneratedTests)

                            finalizer.currentTest = Some(TestPrettyPrinter.javaCodeFor(prefix, suffix, otherSuffix, "GeneratedTest", TestPrettyPrinter.NoOutputVectors))
                            // 下边就是测试部分
                            tsOracle.analyzeTest(prefix, suffix, otherSuffix)
                        })
                    }
                    if (suffixes.size % 5 == 0) {
                        stats.print
                        stats.timer.print
                    }
                }
                case None => // ignore
            }
        }

        println("ClassTester: Could not find bug.")	
    }

    private def getPrefix: Prefix = {
        // try to create a new prefix
        // 
        // 如果prefixes没有达到上限，尝试创建一个新的前缀，InstantiateCutTask
        if (prefixes.size < config.maxPrefixes) { // try to create a new prefix
            new InstantiateCutTask(global).run match {
                case Some(prefix) => {
                    prefix.fixCutVariable
                    assert(prefix.types.contains(config.cut), prefix.types)
                    
                    // 判断是否有参数和prefix等价
                    // 如果等价修改prefixes的大小
                    if (prefixes.exists(oldPrefix => prefix.equivalentTo(oldPrefix))) { 

                        // we re-created an old prefix, return a random existing prefix 
                        // 我们重新创建了一个旧的前缀，返回一个随机的现有前缀

                        //exists会执行n次？

                        //在判断过程中发生了Sequences crashes
                        return prefixes(random.nextInt(prefixes.size))
                    } else {
                        // 所有参数和prefix都不等价

                        // 扩展prefix，扩展了什么？
                        val extendedPrefix = appendStateChangers(prefix)
                        prefixes.add(extendedPrefix)
                        
                        prefix2SuffixGen.put(extendedPrefix, new SuffixGen(extendedPrefix, maxSuffixLength, global))
                        prefix2Suffixes.put(extendedPrefix, new ArrayList[Suffix]())
                        println("New prefix:\n" + extendedPrefix)
                        return extendedPrefix
                    }
                }
                case None => {
                    if (prefixes.isEmpty) {
                        config.checkerListeners.foreach(_.appendResultMsg("Cannot instantiate " + config.cut + ". Is there a public constructor or method to create it?"))
                        finalizer.finalizeAndExit(false)
                        return null
                    } else prefixes(random.nextInt(prefixes.size))
                }
            }
        } else {
            return prefixes(random.nextInt(prefixes.size))
        }
    }

    private def appendStateChangers(prefix: Prefix) = {
        var currentSequence = prefix
        // 从1到5
        for (_ <- 1 to maxStateChangersInPrefix) {
            val stateChangerTask = new StateChangerTask(currentSequence, global)
            stateChangerTask.run match {
                // extendedSequence的类型应当是stateChangerTask.run 的返回值类型CallSequence
                case Some(extendedSequence) => {
                    // println("打个点，看看执行到哪了，currentSequence被赋值几次？")
                    
                    currentSequence = extendedSequence
                    println("想看看currentSequence每次的值是什么" + extendedSequence)
                    // 每次都在叠加，而不是修改，很有意思，可能不是叠加，而是extendedSequence每次都在修改
                    // 果然，是extendedSequence每次都在修改
                    // 说明stateChangerTask.run在修改extendedSequence的值

                }
                case None => // ignore
            }
        }
        // 只返回最后一次的结果？
        currentSequence
    }
}

object ClassTester extends Finalizer {

    val startTime = System.currentTimeMillis
    var stats: Stats = _
    var config: Config = _

    def main(args: Array[String]): Unit = {
        println("Starting ClassTester at " + new Date())
        
        assert(args.size == 6 || args.size == 7 || args.size ==8)
        val cut = args(0)
        

        def isLong(argsStr: String): Boolean = (allCatch opt argsStr.toLong).isDefined

        if(args.size==8){
          args(7) match
          { 
            case time if(isLong(time)) => scheduleTimer(time.toLong)
            case _ => //do nothing as supplied string is not parseable to Integer
           }
          }
        

        val seed = args(2).toInt
        val maxSuffixGenTries = args(3).toInt
        assert(maxSuffixGenTries >= 2)
        val callClinit = args(5).toBoolean
        val selectedCUTMethods: Option[ArrayList[String]] = if (args.size == 7 && args(6)!="-1") Some(readMethods(args(6))) else None
        if (selectedCUTMethods.isDefined) println("Focusing on "+selectedCUTMethods.get.size+" CUT methods")
        else println("No specific CUT methods selected")
        // 输出了"No specific CUT methods selected"，因为只有6个参数
        config = new Config(cut, seed, maxSuffixGenTries, selectedCUTMethods, new File("/tmp/"), callClinit)
        val random = new Random(seed)

        val resultFileName = args(4)
        config.addCheckerListener(new ResultFileCheckerListener(resultFileName))

        val envTypes = new ArrayList[String]
        envTypes.add("java.lang.Object")
        Util.addEnvTypes(args(1), envTypes, this.getClass.getClassLoader)

        stats = new Stats

        val tester = new ClassTester(config, stats, getClass.getClassLoader, ".", envTypes, random, this)
        println("Testing " + cut + " with seed " + seed)
        stats.timer.start("all")

        tester.run
       
        finalizeAndExit(false)
    }

    //function to schedule timer to exit the program after given time in milliseconds.
    def scheduleTimer(time:Long){
      println("Timer scheduled for "+time+" milliseconds")
      val timer = new java.util.Timer()
        timer.schedule(new java.util.TimerTask(){
          def run() {
           println("Time exhausted, terminating the program.")
           finalizeAndExit(false)
           }    
        }, time)//1hr = 3600000
    }
    
    def finalizeAndExit(bugFound: Boolean) = {
        stats.timer.stop("all")
        stats.print
        stats.timer.print
        println("Done with ClassTester at " + new Date)
        val secondsTaken = (System.currentTimeMillis - startTime) / 1000
        config.checkerListeners.foreach(_.appendResultMsg("Time (seconds): " + secondsTaken))

        if (bugFound) {
            val testCode = currentTest.get
            config.checkerListeners.foreach(_.notifyDoneAndBugFound(testCode))
        } else config.checkerListeners.foreach(_.notifyDoneNoBug)

        System.exit(0)
    }

    private def readMethods(fileName: String) = {
        val result = new ArrayList[String]()
        val r = new BufferedReader(new FileReader(fileName))
        var line = r.readLine
        while (line != null) {
            result.add(line)
            line = r.readLine
        }
        r.close
        result
    }

}
