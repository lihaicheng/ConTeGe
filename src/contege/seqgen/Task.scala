package contege.seqgen

import contege.Timer
import contege.Config
import contege.Stats
import contege.GlobalState
import contege.PathTesterConfig

/**
 * Some part of work to do for generating tests.
 * 生成测试的一部分工作。
 * Tries to accomplish the task a configurable nb of 
 * times and returns an extended call sequence if a sequence
 * is found that executes without crashes.
 * 尝试以可配置的nb时间完成任务，如果发现执行的序列没有崩溃，则返回扩展调用序列。
 */
abstract class Task[CallSequence <: AbstractCallSequence[_]](global: GlobalState) {

	private val maxTries = 10
	
	def run: Option[CallSequence] = {
		var triesLeft = maxTries
		while (triesLeft > 0) {
			triesLeft -= 1
			// 计算候选序列， 对于不同的子类，分别实现了不同的方法
			computeSequenceCandidate match {
				case Some(cand) => {
//					println(" -- Running candidate ("+this.getClass.getSimpleName+"):\n"+cand)
					global.stats.timer.start("candExec")
					val successful = global.seqMgr.checkAndRemember(cand)
					global.stats.timer.stop("candExec")
					if (successful) {
//						println(" -- succeeds")
						return Some(cand)
					} else {
//						println(" -- failed")	
					}
				}
				case None => {
					// continue search
				}
			}
		}
		println("Reached maxTries ("+maxTries+")")
		return None
	}
	
	/**
	 * Returns a sequence candidate (which may fail when executed) 
	 * or none of no candidate was found. 
	 * 返回序列候选项(执行时可能失败)或没有找到任何候选项。
	 */
	def computeSequenceCandidate: Option[CallSequence]
	

}