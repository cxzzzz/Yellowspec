import Chisel._
import Chisel.iotesters.PeekPokeTester
import yellowspec._


class ActionMethodProducer extends Module with Yellowspec {

	val io = IO(new Bundle {
		val read = ActionMethodIO(Void , UInt(3.W)) // (param,return)
	})

	val fifoIO = Wire(Decoupled(UInt(3.W)))

	val fifo = Queue(fifoIO, 30)


	io.read := ActionMethod(fifo.valid){
	//method(io.read)(fifo.valid)(
		(params: io.read.paramsType) => {
				fifo.deq()
		}

	} .default {
		fifo.nodeq()
	}

	val num = Reg(UInt(3.W))
	num := 0.U


	rule(fifoIO.ready) {

		when( num > 5.U) {

			fifoIO.enq(num)
		}.otherwise {
			fifoIO.noenq
		}

		num := num + 1.U

	}.default {
		fifoIO.noenq()
	}


}

class MultiActionConsumers extends Module with Yellowspec {

	val io = IO(new Bundle {
		val read = Flipped(ActionMethodIO(Void, UInt(3.W)))
	}
    )
	
	val outs = ActionMethodArbiter(io.read,3)


	val valid = Reg(Bool())
	val data = Reg(UInt(3.W))

	val cnt = Reg(UInt(10.W))

	val cycle = Counter(3)

	rule(cycle.value === 0.U){
		Chisel.printf("0:%d:%d\n", cnt, outs(0)(Void))
		cycle.inc()
	}

	rule( cycle.value === 1.U) {
		Chisel.printf("1:%d:%d\n", cnt, outs(1)(Void))
		cycle.inc()
	}
	rule( cycle.value === 2.U) {
		Chisel.printf("2:%d:%d\n", cnt, outs(2)(Void))
		cycle.inc()
	}

	rule() {
		cnt := cnt + 1.U
	}


}

class ActionMethodArbiterTestTop extends Module {

	val io = IO(new Bundle {})


	val p = Module(new ActionMethodProducer)
	val c = Module(new MultiActionConsumers)

	c.io.read <> p.io.read

	Flipped(Decoupled(0.U))
}

class ActionMethodArbiterTestTopUnitTester(c: ActionMethodArbiterTestTop) extends PeekPokeTester(c) {

	private val gcd = c

	for (i <- 1 to 40) {
		step(1)
	}
}


object ActionMethodArbiterTopTest extends App {


	chisel3.iotesters.Driver.execute(args, () => new ActionMethodArbiterTestTop) {
		c => new ActionMethodArbiterTestTopUnitTester(c)
	}
	//println(chisel3.Driver.emitVerilog( new Top))
}