//

package yellowspec

import chisel3._
import chisel3.util._

trait MethodIO {

	val valid : Bool = Output(Bool())
	def isValid : Bool = valid
	def nonValid : Bool = !valid

}

abstract class AtomicMethodIO[PT <: Data, VT <: Data](paramsGen:PT, valuesGen:VT) extends Bundle with MethodIO{

	//val valid : Bool = Output(Bool())
	val params: PT = Input(paramsGen)
	val values: VT = Output(valuesGen)

	type paramsType = PT
	type valuesType = VT

	//def isValid : Bool = valid
	//def nonValid : Bool = !valid

	/*def := ( method: (this.type) => YContext[VT] ) : YContext[VT] = {
		method( this )
	}
	 */
}

class ActionMethodIO[PT <: Data, VT <: Data](paramsGen: PT, valuesGen: VT)
		extends  AtomicMethodIO[PT, VT](paramsGen,valuesGen) {


	val ready :Bool = Input(Bool())

	def :=(that: ActionMethodIO[PT, VT]): Unit = {
		valid := that.valid
		values := that.values

		that.params := params
		that.ready := ready
	}

	def apply(params: PT)(implicit validStack: MutableOpt[ValidStack]): VT = {

		if (validStack.isEmpty) {
			throw new WrongMethodCallException {
				println(" method should be called in another method or rule(can't in default condition)")
			}
		}

		this.params := params

		ready := true.B

		validStack.get.push(valid)


		values
	}

	def maybe(params:PT): MaybeIO[VT] = {

		val m = Wire(MaybeIO(values))
		 m.valid := valid
		 m.values := values
		 this.params := params
		 this.ready := true.B
		 m
	}


	def toDecoupledIO: DecoupledIO[VT] = {
		require( this.params.getWidth == 0 ,"can't convert Method which has params to DecoupledIO")

		val d = Wire( Decoupled(valuesGen) )
		d.bits := this.values
		d.valid := this.valid
		this.ready := d.ready

		d
	}

	def := ( partialMethod: PartialMethod[PT,VT,ActionMethodIO[PT,VT]]  ) : YContext[VT] = {
		partialMethod.method(this)
	}


	override def cloneType = new ActionMethodIO(paramsGen, valuesGen)
			.asInstanceOf[this.type]

}

class ValueMethodIO[PT <: Data, VT <: Data](paramsGen: PT, valuesGen: VT)
		extends  AtomicMethodIO[PT, VT](paramsGen,valuesGen) {


	def :=(that: ValueMethodIO[PT, VT]): Unit = {
		valid := that.valid
		values := that.values

		that.params := params
	}

	def apply(params: PT)(implicit validStack: MutableOpt[ValidStack]): VT = {

		if (validStack.isEmpty) {
			throw new WrongMethodCallException {
				println(" method should be called in another method or rule(can't in default condition)")
			}
		}

		this.params := params


		validStack.get.push(valid)


		values
	}

	def maybe(params:PT): MaybeIO[VT] = {

		val m = Wire(Maybe(values))
		m.valid := valid
		m.values := values
		this.params := params
		m
	}


	def toValidIO: ValidIO[VT] = {

		require( this.params.getWidth == 0,"can't convert Method which has params to DecoupledIO")

		val d = Wire( Valid(valuesGen) )
		d.bits := this.values
		d.valid := this.valid

		d
	}

	def := ( partialMethod: PartialMethod[PT,VT,ValueMethodIO[PT,VT]]  ) : YContext[VT] = {
		partialMethod.method(this)
	}

	override def cloneType = new ValueMethodIO(paramsGen, valuesGen)
			.asInstanceOf[this.type]

}

class DelayMethodIO[ APT <: Data,AVT <: Data,RPT <: Data,RVT <: Data,RMT <: AtomicMethodIO[RPT,RVT]]
( val request : ActionMethodIO[APT,AVT] , val response:RMT  ) extends Bundle with MethodIO{

	//val readyLock = Input(Bool())

	/*
	def apply( responseAction: (RDT) => Unit )(implicit validStack: MutableOpt[ValidStack])  = {
		validStack.get.callBackQueue.append( (response.valid, responseAction(response.values))
				readyLock := true.B
	}
	 */
	def apply( responseAction: (RVT) => Unit )(implicit validStack: MutableOpt[ValidStack])
		= ???


}


class PartialMethod[PT <:Data,VT <: Data,MT <: AtomicMethodIO[PT,VT]]( md: (MT) => YContext[VT]){
	var method = md
	var method2 = md
	def default( block: => Unit) :PartialMethod[PT,VT,MT] ={
		this.method = ( MT )=> {
			val ycontext = this.method2( MT )
			ycontext.default(block)
			ycontext
		}
		this
	}
}


object ActionMethodIO {

	def apply[PT <: Data, VT <: Data](paramsGen: PT, valuesGen: VT): ActionMethodIO[PT, VT] = {
		new ActionMethodIO(paramsGen.cloneType, valuesGen.cloneType)
	}

	def apply[VT <: Data]( rv : ReadyValidIO[VT]): ActionMethodIO[VoidType.Void, VT] = {
		 new ActionMethodIO[VoidType.Void,VT](VoidType().cloneType , rv.bits.cloneType)
	}


	def toDecoupledIO[PT <: Data, VT <: Data](method: ActionMethodIO[PT,VT]): DecoupledIO[VT] =
		method.toDecoupledIO
}


object ActionMethod{

	def apply[VT <: Data]( rv : ReadyValidIO[VT]): ActionMethodIO[VoidType.Void, VT] = {

		val method = Wire( ActionMethodIO( rv))
		rv.ready := method.ready
		method.valid := rv.valid
		method.values := rv.bits
		method
	}


	def apply[PT <:Data,VT <: Data]( cond: => Bool = true.B)( block: (PT) => VT)
								   (implicit currentValidStack:MutableOpt[ValidStack])
	: PartialMethod[PT,VT,ActionMethodIO[PT,VT]] = {
		new PartialMethod( (io) => new YContext[VT] (
			cond, io.ready, block (io.params), currentValidStack,
					io.valid, io.values
		)
		)
	}

}

object ValueMethodIO {

	def apply[PT <: Data, VT <: Data](paramsGen: PT, valuesGen: VT): ValueMethodIO[PT, VT] = {
		new ValueMethodIO(paramsGen.cloneType, valuesGen.cloneType)
	}

	def apply[VT <: Data]( v : ValidIO[VT]): ValueMethodIO[VoidType.Void, VT] = {
		new ValueMethodIO[VoidType.Void,VT](VoidType().cloneType , v.bits.cloneType)
	}


	def toValidIO[PT <: Data, VT <: Data](method: ValueMethodIO[PT,VT]): ValidIO[VT] =
		method.toValidIO
}

object ValueMethod {
	def apply[VT <: Data]( v : ValidIO[VT]): ValueMethodIO[VoidType.Void, VT] = {
		val method = Wire( ValueMethodIO(v))
		method.valid := v.valid
		method.values := v.bits
		method
	}


	def apply[PT <:Data,VT <: Data]( cond: => Bool = true.B)( block: (PT) => VT)
																 (implicit currentValidStack:MutableOpt[ValidStack])
	: PartialMethod[PT,VT,ValueMethodIO[PT,VT]] = {
		new PartialMethod( (io) => new YContext[VT] (cond, true.B , block (io.params), currentValidStack,
					io.valid, io.values)
		)
	}
}

object DelayMethodIO {

	def apply[APT <: Data,AVT <: Data,RPT <: Data,RVT <: Data,RMT <: AtomicMethodIO[RPT,RVT]]
	(request:ActionMethodIO[APT,AVT],response:RMT): DelayMethodIO[APT,AVT,RPT,RVT,RMT]= {
		new DelayMethodIO( request,response)
	}

}


