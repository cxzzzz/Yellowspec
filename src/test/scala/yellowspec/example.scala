
import chisel3._
import yellowspec._

class Mult2(depth: Int) extends Module with Yellowspec {
	val io = IO(new Bundle {
		// declare a method Mult2 own
		val deq = ActionMethodIO(NoneParam, UInt(8.W))
		// declare a method Mult2 use
		val enq = Flipped(ActionMethodIO(NoneParam, UInt(8.W)))
	})

	val queue = Reg(Vec(depth, UInt(8.W)))
	val headPointer = Reg(UInt(3.W))
	val tailPointer = Reg(UInt(3.W))

	// bind io.deq to a method , which take a number from queue and return if queue is not empty
	method(io.deq)(headPointer =/= tailPointer) {
		(NoneParam) => {
			tailPointer := tailPointer + 1.U
			queue(tailPointer)
		}
	}

	// when the rule queue is not full , read a number , multiply it by 2 and put it in to the queue
	rule(headPointer =/= tailPointer - 1.U) {
		headPointer := headPointer + 1.U
		queue(headPointer) := io.enq(NoneParam) * 2.U
	}

}

