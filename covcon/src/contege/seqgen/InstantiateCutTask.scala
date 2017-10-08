package contege.seqgen

import scala.collection.JavaConversions._
import scala.collection.mutable.Set
import scala.collection.mutable.Map
import java.util.ArrayList
import contege._

/**
  * Adds calls to instantiate the CUT (e.g. a call to a constructor
  * and calls to provide arguments for the constructor call).
  * 添加实例化cut的调用
  * 例如对构造函数的调用和为构造函数调用提供参数的调用
  *
  */
class InstantiateCutTask(global: GlobalState) extends Task[Prefix](global)
{
	
	def computeSequenceCandidate: Option[Prefix] =
	{
		val cut = global.config.cut
		//constructingAtoms被执行的操作的序列，操作，指的是什么？
		//构建一组操作
		val constructingAtoms = new ArrayList[Atom]
		//构造器
		//constructors是Seq[ConstructorAtom]类型，一组方法的集合
		val constructors = global.typeProvider.constructors(cut)
		
		if (global.config.isInstanceOf[SubclassTesterConfig])
		//判断global.config是否是SubclassTesterConfig类型的
		{
			// use only constructors that can be mapped to subclass constructors
			//如果是
			//将global.config对象类型强制转换为SubclassTesterConfig类型。
			//为什么已经是这种类型还需要强转一次
			val subclassTesterConfig = global.config.asInstanceOf[SubclassTesterConfig]
			//c.paramTypes得到一个Seq[String]参数类型，，参数指什么？
			//mkString将Seq[String]添加（）的前后缀，并用“，”分割
			
			val mappableConstructors = constructors.filter(c => subclassTesterConfig.constructorMap.mappable(c.paramTypes.mkString("(", ",", ")")))
			//为constructingAtoms添加了一个词条
			constructingAtoms.addAll(mappableConstructors)
		}
		else
		{
			// use all constructors and methods that can instantiate the CUT
			//使用可以实例化cut的所有构造函数和方法
			constructingAtoms.addAll(constructors)
			val ownStaticCreators = global.typeProvider.cutMethods.filter(m => m.isStatic && m.returnType.isDefined && m.returnType.get == cut)
			constructingAtoms.addAll(ownStaticCreators)
			if (global.config.isInstanceOf[PathTesterConfig])
			{
				// use all methods that return an instance of the CUT
				val otherCreators = global.typeProvider.allAtomsGivingType(cut)
				constructingAtoms.addAll(otherCreators)
			}
			else
			{
				// use static methods only (why? mostly to preserve ClassTester's behavior as described in PLDI'12)
				val otherStaticCreators = global.typeProvider.allAtomsGivingType(cut).filter(atom => atom.isMethod && atom.isStatic)
				constructingAtoms.addAll(otherStaticCreators)
			}
		}
		
		if (constructingAtoms.isEmpty)
		{
			//constructingAtoms为空，不能创建cut的instance
			//没有构造函数或方法来创建cut实例！
			println("No constructor or method to create instance of CUT! " + cut)
			return None
		}
		//取出一个随机的constructor（构造函数）
		val constructor = constructingAtoms(global.random.nextInt(constructingAtoms.size))
		
		var result = new Prefix(global)  // create a fresh sequence创建一个新的序列
		
		// if the "constructor" is an instance method, create a subtask to find an instance
		// 如果“构造函数”是一个实例方法，那么就创建一个子任务来查找实例
		var receiver: Option[Variable] = None
		//如果 constructor的修饰符不为静态(static) 也不为？isConstructor？
		if (!constructor.isStatic && !constructor.isConstructor)
		{
			val receiverType = constructor.declaringType
			//创建一个子任务来查找实例
			val receiverTask = new GetParamTask(result, receiverType, false, global)
			//得到的extendedSequence就是传进去的result
			receiverTask.run match
			{
				case Some(extendedSequence) =>
				{
					result = extendedSequence
					assert(receiverTask.param.isDefined)
					receiver = receiverTask.param
				}
				case None => return None
			}
		}
		
		// create a subtask for each parameter; if one fails, this task also fails
		//为每个参数创建一个子任务;如果一个失败了，这个任务也会失败
		val args = new ArrayList[Variable]()
		constructor.paramTypes.foreach(typ =>
		{
			val paramTask = new GetParamTask(result, typ, true, global)
			paramTask.run match
			{
				case Some(extendedSequence) =>
				{
					result = extendedSequence
					assert(paramTask.param.isDefined)
					args.add(paramTask.param.get)
				}
				case None =>
				{
					return None
				}
			}
		})
		//result就是prefix
		result.appendCall(constructor, receiver, args, Some(new ObjectVariable), None, true)
		
		Some(result)
	}
	
}