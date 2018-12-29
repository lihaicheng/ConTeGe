package contege.seqgen

import scala.collection.JavaConversions._
import scala.collection.mutable.Set
import scala.collection.mutable.Map
import java.util.ArrayList
import contege._

/**
 * Adds calls to instantiate the CUT (e.g. a call to a constructor
 * and calls to provide arguments for the constructor call).
 * 添加实例化CUT的调用(例如对构造函数的调用和为构造函数调用提供参数的调用)。
 */
class InstantiateCutTask(global: GlobalState) extends Task[Prefix](global) {

    def computeSequenceCandidate: Option[Prefix] = {
        val cut = global.config.cut
        // 构建原子，，是什么含义 Atom：要执行的操作(例如，方法调用或字段访问)。
        val constructingAtoms = new ArrayList[Atom]
        val constructors = global.typeProvider.constructors(cut)
        if (global.config.isInstanceOf[SubclassTesterConfig]) {
            // use only constructors that can be mapped to subclass constructors
            // 只使用可以映射到子类构造函数的构造函数
            val subclassTesterConfig = global.config.asInstanceOf[SubclassTesterConfig]
            val mappableConstructors = constructors.filter(c => subclassTesterConfig.constructorMap.mappable(c.paramTypes.mkString("(", ",", ")")))
            constructingAtoms.addAll(mappableConstructors)
        } else {
            // use all constructors and methods that can instantiate the CUT
            // 使用所有可以实例化CUT的构造函数和方法
            constructingAtoms.addAll(constructors)
            val ownStaticCreators = global.typeProvider.cutMethods.filter(m => m.isStatic && m.returnType.isDefined && m.returnType.get == cut)
            constructingAtoms.addAll(ownStaticCreators)

            // use all methods that return an instance of the CUT
            // 使用所有返回CUT实例的方法
            val otherCreators = global.typeProvider.allAtomsGivingType(cut)
            // PLDI'12 behavior: use static methods only:
            // val otherCreators = global.typeProvider.allAtomsGivingType(cut).filter(atom => atom.isMethod && atom.isStatic)
            constructingAtoms.addAll(otherCreators)
        }

        if (constructingAtoms.isEmpty) {
            println("No constructor or method to create instance of CUT! " + cut)
            return None
        }

        val constructor = constructingAtoms(global.random.nextInt(constructingAtoms.size))

        var result = new Prefix(global) // create a fresh sequence

        // if the "constructor" is an instance method, create a subtask to find an instance
        // 如果“构造函数”是实例方法，则创建一个子任务来查找实例
        var receiver: Option[Variable] = None
        if (!constructor.isStatic && !constructor.isConstructor) {
            val receiverType = constructor.declaringType
            val receiverTask = new GetParamTask(result, receiverType, false, global)
            receiverTask.run match {
                case Some(extendedSequence) => {
                    result = extendedSequence
                    assert(receiverTask.param.isDefined)
                    receiver = receiverTask.param
                }
                case None => return None
            }
        }

        // create a subtask for each parameter; if one fails, this task also fails
        // 为每个参数创建子任务;如果一个失败，这个任务也会失败
        val args = new ArrayList[Variable]()
        constructor.paramTypes.foreach(typ => {
            val paramTask = new GetParamTask(result, typ, true, global)
            paramTask.run match {
                case Some(extendedSequence) => {
                    result = extendedSequence
                    assert(paramTask.param.isDefined)
                    args.add(paramTask.param.get)
                }
                case None => {
                    return None
                }
            }
        })

        result.appendCall(constructor, receiver, args, Some(new ObjectVariable), None, true)

        Some(result)
    }

}