//

package yellowspec

import chisel3._
import chisel3.util._


abstract class MethodIO[PT <: Data, VT <: Data](paramsGen:PT,valuesGen:VT) extends Bundle{

	val valid : Bool = Output(Bool())
	val params: PT = Input(paramsGen)
	val values: VT = Output(valuesGen)

	def isValid : Bool = valid
	def nonValid : Bool = !valid

}

class ActionMethodIO[PT <: Data, VT <: Data](paramsGen: PT, valuesGen: VT)
		extends  MethodIO[PT, VT](paramsGen,valuesGen) {


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

		val m = Wire(Maybe(values))
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

	override def cloneType = new ActionMethodIO(paramsGen, valuesGen)
			.asInstanceOf[this.type]

}

class ValueMethodIO[PT <: Data, VT <: Data](paramsGen: PT, valuesGen: VT)
		extends  MethodIO[PT, VT](paramsGen,valuesGen) {


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

	override def cloneType = new ValueMethodIO(paramsGen, valuesGen)
			.asInstanceOf[this.type]

}

object ActionMethodIO {

	def apply[PT <: Data, VT <: Data](paramsGen: PT, valuesGen: VT): ActionMethodIO[PT, VT] = {
		new ActionMethodIO(paramsGen.cloneType, valuesGen.cloneType)
	}

	def apply[VT <: Data]( rv : ReadyValidIO[VT]): ActionMethodIO[NoneWire.NoneWire, VT] = {
		 new ActionMethodIO[NoneWire.NoneWire,VT](NoneWire().cloneType , rv.bits.cloneType)
	}


	def toDecoupledIO[PT <: Data, VT <: Data](method: ActionMethodIO[PT,VT]): DecoupledIO[VT] =
		method.toDecoupledIO
}


object ActionMethod{

	def apply[VT <: Data]( rv : ReadyValidIO[VT]): ActionMethodIO[NoneWire.NoneWire, VT] = {

		val method = Wire( ActionMethodIO( rv))
		rv.ready := method.ready
		method.valid := rv.valid
		method.values := rv.bits
		method
	}


}

object ValueMethodIO {

	def apply[PT <: Data, VT <: Data](paramsGen: PT, valuesGen: VT): ValueMethodIO[PT, VT] = {
		new ValueMethodIO(paramsGen.cloneType, valuesGen.cloneType)
	}

	def apply[VT <: Data]( v : ValidIO[VT]): ValueMethodIO[NoneWire.NoneWire, VT] = {
		new ValueMethodIO[NoneWire.NoneWire,VT](NoneWire().cloneType , v.bits.cloneType)
	}


	def toValidIO[PT <: Data, VT <: Data](method: ValueMethodIO[PT,VT]): ValidIO[VT] =
		method.toValidIO
}

object ValueMethod {
	def apply[VT <: Data]( v : ValidIO[VT]): ValueMethodIO[NoneWire.NoneWire, VT] = {
		val method = Wire( ValueMethodIO(v))
		method.valid := v.valid
		method.values := v.bits
		method
	}
}
