package contege.seqgen

import contege.Timer
import contege.Config
import contege.Stats
import contege.GlobalState
import contege.PathTesterConfig

/**
  * Some part of work to do for generating tests.
  * Tries to accomplish the task a configurable nb of
  * times and returns an extended call sequence if a sequence
  * is found that executes without crashes.
  */
abstract class Task[CallSequence <: AbstractCallSequence[_]](global: GlobalState)
{
	
	private val maxTries = 10
	
	def run: Option[CallSequence] =
	{
		var triesLeft = maxTries
		while (triesLeft > 0)
		{
			triesLeft -= 1
			computeSequenceCandidate match
			{
				case Some(cand) =>
				{
					//Some(cand)就是从函数中返回的结果
					val successful = global.seqMgr.checkAndRemember(cand)
					//判断这个序列是否可用
					if (successful)
					{
						return Some(cand)
					}
				}
				case None =>
				{
					// continue search
					//最多检查10遍，查不出来就返回None
				}
			}
		}
		println("Reached maxTries (" + maxTries + ")")
		return None
	}
	
	/**
	  * Returns a sequence candidate (which may fail when executed)
	  * or none of no candidate was found.
	  * 返回一个序列候选者(在执行时可能会失败)，或者没有找到一个候选对象。
	  */
	def computeSequenceCandidate: Option[CallSequence]
	
}