package contege
import java.io.PrintWriter
// 你可以定义一个抽象类，它定义了一些方法但没有实现他们。取而代之的是有扩展抽象类的子类定义这些方法。
abstract class CheckerListener {
  
  def updateNbGeneratedTests(nb: Long)

  /**
   * Some output of the checker that may be of interest
   * to users.
   * (Not for debugging output,
   * not for reporting details about a bug found.)
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
