package contege
// 类似接口
trait Finalizer {
	//Scala Option(选项)类型用来表示一个值是可选的（有值或无值)。
	//Option[T] 是一个类型为 T 的可选值的容器： 如果值存在， Option[T] 就是一个 Some[T] ，如果不存在， Option[T] 就是对象 None 。
    var currentTest: Option[String] = None
    
	def finalizeAndExit(bugFound: Boolean)
}