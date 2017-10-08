package contege

import scala.collection.mutable.Set
import java.lang.reflect._

/**
  * Reads methods, constructors, and fields of a class,
  * ignoring all members that can't be called in a generated test
  * (i.e., only public, non-abstract etc. members are considered).
  * 读取方法、构造函数和类的字段，
  * 忽略在生成的测试中不能调用的所有成员
  * (即。只有公共的、非抽象的等成员被考虑)。
  */
class ClassReader(val cls: Class[_])
{
	
	def readMethodAtoms =
	{
		val atoms = Set[MethodAtom]()
		cls.getMethods().foreach(m =>
			if (Modifier.isPublic(m.getModifiers) &&
					//Modifier修饰符
					//判断m方法是否为public
					!m.isSynthetic &&
					//不是由Java语言规范定义的合成方法，应该指的是get，set等方法
					!Modifier.isAbstract(m.getModifiers) &&
					//不是抽象方法
					m.getName != "XXXmyClinitXXX" &&
					//方法名不是XXXmyClinitXXX
					m.getDeclaringClass.getName != "java.lang.Object" &&
					//方法所在的类不是java.lang.Object
					!ExcludedMethods.methods.contains(m.toString)
					//ExcludedMethods的methods（）set里不包含方法m
			)
				atoms.add(new MethodAtom(cls, m))
		)
		atoms.toSeq.sortWith((x, y) => x.toString < y.toString)
		//val cfpPair = Set[MethodAtom] ()
	}
	
	def readConstructorAtoms =
	{
		val atoms = Set[ConstructorAtom]()
		if (!Modifier.isAbstract(cls.getModifiers))
		{
			//cls不是抽象类
			//getConstructors方法返回一个Constructor对象
			cls.getConstructors.foreach(c =>
				if (Modifier.isPublic(c.getModifiers) &&
					//构造器是public类型
					!c.isSynthetic &&
					//c不是Java语言规范定义的一种合成类
					!Modifier.isAbstract(c.getModifiers))
					//c不是抽象类
					{
						atoms += new ConstructorAtom(cls, c)
						//c是就是cls的构造方法是一个方法
					}
					
			)
		}
		atoms.toSeq.sortWith((x, y) => x.toString < y.toString)
	}
	
	def readFieldGetterAtoms =
	{
		val atoms = Set[FieldGetterAtom]()
		cls.getFields.foreach(f => if (Modifier.isPublic(f.getModifiers) &&
				!f.isSynthetic &&
				!Modifier.isAbstract(f.getModifiers)) atoms += new FieldGetterAtom(cls, f))
		atoms.toSeq.sortWith((x, y) => x.toString < y.toString)
	}
	
}
