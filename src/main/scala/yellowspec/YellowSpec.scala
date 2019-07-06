
package yellowspec

import chisel3._
import chisel3.util._

import scala.collection.mutable.Stack


protected object NoneWire {
	def apply(): Bits = {
		val noneWire = Wire(Bits(0.W))
		noneWire := 0.U(1.W).asBits
		noneWire
	}

	type NoneWire = Bits
}


protected class ValidInfo(val valid: Bool, val depth: Int)

protected object YSpecMode extends Enumeration{

	val Chisel , Yspec = Value
}

protected class ValidStack extends Stack[ValidInfo] {

	var currentDepth: Int = 0

	private var _runCWhen = true

	var _mode = YSpecMode.Yspec

	def  mode = _mode

	def  mode( m : YSpecMode.Value):Unit =  {
		_mode = m
		this.clear
	}

	def runCWhen(value: Boolean) = _runCWhen = value

	def valid: Bool = {
		assert(this.length == 1)
		this.top.valid
	}

	def push(valid: => Bool): Unit = {
		this.push(new ValidInfo(valid, currentDepth))
	}


	def depthInc(): Unit = {
		currentDepth += 1
	}

	def depthDec(): Unit = {
		currentDepth -= 1
	}

	def mergeDepth(op: (Bool, Bool) => Bool)(depth: Int): Unit = {
		while (this.length > 1 && this.top.depth >= depth && this (1).depth >= depth) {
			val v1 = this.pop
			val v2 = this.pop
			assert(v2.depth <= v1.depth)
			this.push(new ValidInfo(op(v1.valid, v2.valid), v2.depth))
		}
	}

	def mergeTop(op: (Bool, Bool) => Bool): Unit = {
		val v1 = this.pop
		val v2 = this.pop
		assert(v2.depth <= v1.depth)
		this.push(new ValidInfo(op(v1.valid, v2.valid), v2.depth))
	}

}

class YWhenContext //( cond: => Bool , block: => Unit , validStack : MutableOpt[ValidStack] , lastCond: => Bool ){
(cwhen: WhenContext, validStack: MutableOpt[ValidStack], lastCond: => Bool = false.B) {

	def selfValid(cond: => Bool): Bool = (!lastCond) && cond

	def elsewise(cond: => Bool)(block: => Unit): YWhenContext = {

		if (validStack.nonEmpty && validStack.get.mode == YSpecMode.Yspec) { //&& ! validStack.get.runCWhen) {
			validStack.get.push(!selfValid(cond))
			val selfDepth = validStack.get.currentDepth
			validStack.get.depthInc()

			//newContext

			chisel3.when(false.B) {
				block
			}

			validStack.get.depthDec()
			validStack.get.push(true.B)
			validStack.get.mergeDepth(_ && _)(selfDepth + 1)

			validStack.get.depthDec()
			validStack.get.mergeTop(_ || _)

			new YWhenContext(null, validStack, lastCond || cond)

		} else {
			val context = cwhen.elsewhen(cond) {
				block
			}
			new YWhenContext(context, validStack, lastCond || cond)
		}

	}

	def otherwise(block: => Unit): Unit = {

		if (validStack.nonEmpty && validStack.get.mode == YSpecMode.Yspec) {

			validStack.get.push(!selfValid(true.B))
			val selfDepth = validStack.get.currentDepth
			validStack.get.depthInc()

			chisel3.when(false.B) {
				block
			}

			validStack.get.push(true.B)
			validStack.get.mergeDepth(_ && _)(selfDepth + 1)

			validStack.get.depthDec()
			validStack.get.mergeTop(_ || _)

		} else {

			val context = cwhen.otherwise {
				block
			}
		}

	}

}

object YWhenContext {

	def when(cond: => Bool, condRev: Boolean = true)(block: => Unit)(implicit validStack: MutableOpt[ValidStack])
	: YWhenContext = {


		if (validStack.nonEmpty && validStack.get.mode == YSpecMode.Yspec ) {


			validStack.get.push(if (condRev) !cond else cond)

			val selfDepth = validStack.get.currentDepth
			validStack.get.depthInc()

			chisel3.when(false.B) {
				block
			}

			validStack.get.push(true.B)

			validStack.get.mergeDepth(_ && _)(selfDepth + 1)
			validStack.get.depthDec()

			validStack.get.mergeTop(_ || _)

			new YWhenContext(null, validStack, cond)

		} else {

			val context = chisel3.when(cond) {
				block
			}
			new YWhenContext(context, validStack, cond)
		}

	}

}

class YContext[VT <: Data](cond: => Bool, outerCond: => Bool, block: => VT,
						   validStack: MutableOpt[ValidStack], selfValid: Bool = null, selfValues: VT = null) {


	validStack.set(new ValidStack)

	var values :VT = null.asInstanceOf[VT]

	val innerCond = {

		validStack.get.mode(YSpecMode.Yspec)

		chisel3.when(false.B) {
			values = Wire(block.cloneType)
		}

		validStack.get.push(true.B)
		validStack.get.mergeDepth(_ && _)(0)

		val cond = {

			assert(validStack.get.length == 1)
			validStack.get.top.valid
		}

		cond
	}

	val valid: Bool = innerCond && cond

	// return  must have a default value
	values := 0.U.asTypeOf(values)


	if (selfValid != null) selfValid := valid
	if (selfValues != null) selfValues := values


	val enable = cond && innerCond && outerCond


	//  default must be run  on top of cwhenContext initialized

	val cwhenContext = {

		validStack.get.mode(YSpecMode.Chisel)

		chisel3.when(enable) {

			//val ret = block
			values := block

		}
	}

	validStack.clear()

	def default(defaultBlock: => Unit): Unit = {

		require(validStack.isEmpty," default shouldn't be in a method or rule")

		cwhenContext.otherwise(defaultBlock)

	}


}


class WrongMethodCallException extends Exception


trait YellowSpec {


	//private var insideRuleOrMethod :Boolean = false
	protected implicit val currentValidStack: MutableOpt[ValidStack] = MutableOpt.from(None)

	protected implicit def toDecoupledIO[PT <: Data, VT <: Data](method: ActionMethodIO[PT,VT]) =
		ActionMethodIO.toDecoupledIO(method)

	protected implicit def toValidIO[PT <: Data, VT <: Data](method: ValueMethodIO[PT,VT]) =
		ValueMethodIO.toValidIO(method)

	val NoneParam = NoneWire()
	val NoneValue = NoneWire()

	def rule(cond: => Bool = true.B)(block: => Unit): YContext[NoneWire.NoneWire] = {


		new YContext[NoneWire.NoneWire](cond, true.B, {
			block;
			NoneWire()
		}, currentValidStack)

	}

	def method[PT <: Data, VT <: Data](io: ActionMethodIO[PT, VT])
									  (cond: => Bool = true.B)(block: (PT) => VT): YContext[VT] = {

		new YContext[VT](cond, io.ready, block(io.params), currentValidStack,
			io.valid, io.values)
	}




	def when(cond: => Bool)(block: => Unit): YWhenContext = YWhenContext.when(cond)(block)

}

