package contege

import scala.collection.JavaConversions._
import scala.collection.mutable.Set
import scala.collection.mutable.Map
import java.util.ArrayList
import java.lang.reflect.Constructor
import contege.seqgen._

/**
 * Computes all sequential interleavings of two suffixes.
 * Both suffixes must share the same prefix.
 * 计算两个后缀的所有顺序交错。
 * 两个后缀必须共享相同的前缀。
 */
class SequentialInterleavings(prefix: Prefix, suffix1: Suffix, suffix2: Suffix) {
    assert(suffix1.prefix == suffix2.prefix && suffix1.prefix == prefix)

    // compute boolean interleavings (relatively cheap)
    // 计算布尔交错(相对便宜)?
    private val booleanInterleavings = new ArrayList[Array[Boolean]]()
    private val emptySlots = new Array[Boolean](suffix1.length + suffix2.length)
    interleavings(emptySlots, suffix2.length, -1)

    // iterator to create real interleavings (i.e. complete call sequences) on demand
    // iterator根据需要创建真正的交错(即完整的调用序列)
    private val booleanInterleavingsIter = booleanInterleavings.iterator

    def nextInterleaving: Option[Prefix] = {
        if (!booleanInterleavingsIter.hasNext) return None
        val booleanInterleaving = booleanInterleavingsIter.next
        assert(booleanInterleaving.length == suffix1.length + suffix2.length)
        val interleaved = prefix.copy
        val oldVar2NewVarSuffix1 = Map[Variable, Variable]()
        val oldVar2NewVarSuffix2 = Map[Variable, Variable]()
        val callIter1 = suffix1.callIterator
        val callIter2 = suffix2.callIterator
        booleanInterleaving.foreach(b => {
            val call = if (!b) callIter1.next else callIter2.next
            // create new vars for ret vals in suffixes to avoid redefs in interleaving
            // 在后缀中创建新的vars，以避免在交错中返回
            val newRetVal = if (call.retVal.isDefined) {
                val newVar = new ObjectVariable
                if (!b) oldVar2NewVarSuffix1.put(call.retVal.get, newVar) else oldVar2NewVarSuffix2.put(call.retVal.get, newVar) 
            	Some(newVar)
            } else call.retVal
            val newReceiver = if (call.receiver.isDefined) {
            	Some((if (!b) oldVar2NewVarSuffix1 else oldVar2NewVarSuffix2).getOrElse(call.receiver.get, call.receiver.get))
            } else call.receiver
            val newArgs = new ArrayList[Variable]
            call.args.foreach(arg => {
                val newArg = (if (!b) oldVar2NewVarSuffix1 else oldVar2NewVarSuffix2).getOrElse(arg, arg)
                newArgs.add(newArg)
            })
            interleaved.appendCall(call.atom, newReceiver, newArgs, newRetVal, call.downcastType)
        })
        assert(!callIter1.hasNext && !callIter2.hasNext)
        assert(interleaved.length == suffix1.length + suffix2.length + prefix.length)
        Some(interleaved)
    }

    // 上下文。suffix（后缀）包含一些元素
    // 参数1 空的顺序表，长度为后缀1、2之和
    // 参数2 后缀2的长度
    // 参数3 最初传入的时候是-1 不知道什么意义
    private def interleavings(t2sSlots: Array[Boolean], remainingForT2: Int, lastForT2: Int): Unit = {
        // 如果 后缀1的长度小于0，（0 > t2sSlots.size  - remainingForT2）return
        if (lastForT2 > t2sSlots.size - 1 - remainingForT2) return

        // 如果后缀2长度等于0，也就是说空槽的长度和s1的一样
        // booleanInterleavings中加入s1.length个false
        if (remainingForT2 == 0) {
            booleanInterleavings.add(t2sSlots)
            return
        }
        // suffix1 >= 0
        // suffix2 > 0
        // newForT2 从0 -> length-1
        for (newForT2 <- lastForT2 + 1 to t2sSlots.size - 1) {
            val slotsCopy = t2sSlots.clone
            slotsCopy(newForT2) = true
            interleavings(slotsCopy, remainingForT2 - 1, newForT2)
        }
    }

}

	