
package yellowspec

import chisel3._
import chisel3.util._

object Maybe{

	def apply[T <:Data](gen : T ,valid : Bool = true.B)  : MaybeIO[T] = {
		val m = Wire(new MaybeIO[T](gen.cloneType))
		m.valid := true.B
		m.values := gen
		m
	}
}

class MaybeIO[T <: Data ]( gen : T ) extends ValidIO(gen) {

	val values :T = bits


	def isDefined :Bool = valid
	def isEmpty : Bool = !isDefined

	def nonEmpty : Bool = isDefined

	def isValid : Bool = isDefined
	def nonValid : Bool = isEmpty

	def get: T = values

	def getOrElse( default: => T): T = Mux( isDefined , get, default )

	def map[B <: Data](f: T => B):MaybeIO[B] = {
		val b = Wire(Maybe(f(get)))
		b.valid := isDefined
		b
	}

	def fold[B <: Data](ifEmpty: => B)(f: T => B):B = Mux( isDefined , f(get), ifEmpty)

	def flatMap[B <:Data ](f: T => MaybeIO[B]):MaybeIO[B] = {
		val b = Wire( f(get) )
		b.valid := isDefined
		b
	}

	def orElse( alternative: => MaybeIO[T] ):MaybeIO[T] =
		Mux( isDefined ,  this, alternative)

	def set():Unit = {
		valid := false.B
	}

	def set( values :T):Unit = {
		valid := true.B
		this.values := values
	}

	override def cloneType  = new MaybeIO(gen).asInstanceOf[this.type]

}
