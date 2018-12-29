package contege.seqexec.reflective

/**
 * Result of executing an Atom reflectively.
 * 反射地执行一个原子的结果。
 */
abstract class ExecutionResult

case class Exception(t: Throwable) extends ExecutionResult

case class Normal(returnValue: Object) extends ExecutionResult

object OK extends ExecutionResult