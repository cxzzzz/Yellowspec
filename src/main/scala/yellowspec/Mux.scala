
package yellowspec

import chisel3._

class YMux[T <: Data](toMux: (T) => T) {

	def otherwise(alt: T): T = {
		toMux(alt)
	}

	def elseswitch(cond: Bool)(con: T): YMux[T] = {

		def f(alt: T) = Mux(cond, con, alt)

		new YMux((alt: T) => toMux(f(alt)))
	}
}

object mux {

	def apply[T <: Data](cond: Bool)(con: T): YMux[T] = {
		new YMux((alt: T) => Mux(cond, con, alt))
	}
}

