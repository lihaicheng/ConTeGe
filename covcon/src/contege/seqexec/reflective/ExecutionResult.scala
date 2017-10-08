package contege.seqexec.reflective

/**
  * Result of executing an Atom reflectively.
  * 执行Atom（一种操作）反射的结果。
  */
abstract class ExecutionResult

case class Exception(t: Throwable) extends ExecutionResult

case class Normal(returnValue: Object) extends ExecutionResult

object OK extends ExecutionResult