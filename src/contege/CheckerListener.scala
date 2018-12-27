package contege
import java.io.PrintWriter

abstract class CheckerListener {
  
  def updateNbGeneratedTests(nb: Long)

  /**
   * Some output of the checker that may be of interest
   * to users.
   * (Not for debugging output,
   * not for reporting details about a bug found.)
   * 检查器的一些输出可能会引起用户的兴趣。(不是用于调试输出，也不是用于报告发现的错误的详细信息。)
   */
  def appendStatusMsg(s: String)
  
  /**
   * Info about bug found.
   */
  def appendResultMsg(s: String)

  /**
   * Called when the checker is done because it has found a bug.
   */
  def notifyDoneAndBugFound(testCode: String)
  
  /**
   * Called when the checker is done because it has reached its
   * stopping criterion (but no bug was found).
   */
  def notifyDoneNoBug
}
