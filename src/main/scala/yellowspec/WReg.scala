
package yellowspec

import  chisel3._

class WReg[T <: Data]( gen:T,init:T = null) {

	val w = WireInit(gen,DontCare)
	val r = if( init != null) {
		RegInit(gen,init)
	}else{
		Reg(gen)
	}

	val writeEn = WireInit(false.B)

	def apply():T = mux(writeEn){w}.otherwise{r}

	def := ( op:(T) => Unit ):Unit = {
		writeEn := true.B
		op(w)
		op(r)
	}

	def := ( that :T ):Unit = {
		writeEn := true.B
		w := that
		r := that
	}

}

object WReg{
	def apply[T <: Data]( gen :T,init:T = null) :WReg[T] = {
		new WReg(gen.cloneType,init)
	}
}
object WRegInit{
	def apply[T <: Data]( gen :T ) :WReg[T] = {
		WReg(gen,gen)
	}
}

