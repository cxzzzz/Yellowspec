import Chisel._
import Chisel.iotesters.PeekPokeTester
import yellowspec._

class ValueMethodProducer extends Module with Yellowspec {

	val io = IO(new Bundle {
		val read = ValueMethodIO(Void , UInt(3.W)) // (param,return)
	})

	val fifoIO = Wire(Decoupled(UInt(3.W)))

	val fifo = Queue(fifoIO, 30)


	io.read := ValueMethod(fifo.valid){
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

class ValueMethodConsumer extends Module with Yellowspec {

	val io = IO(new Bundle {
		val read = Flipped(ValueMethodIO(Void, UInt(3.W)))
	}
	)

	val valid = Reg(Bool())
	val data = Reg(UInt(3.W))

	val cnt = Reg(UInt(10.W))

	rule() {
		Chisel.printf("%d:%d\n", cnt, io.read(Void))
	}

	rule() {
		cnt := cnt + 1.U
	}


}
class ValueMethodTestTop extends Module {

	val io = IO(new Bundle {})


	val p = Module(new ValueMethodProducer)
	val c = Module(new ValueMethodConsumer)

	c.io.read <> p.io.read

	Flipped(Decoupled(0.U))
}

class ValueMethodTestTester(c: ValueMethodTestTop) extends PeekPokeTester(c) {

	private val gcd = c

	for (i <- 1 to 40) {
		step(1)
	}
}


object ValueMethodTestTester extends App {

	chisel3.iotesters.Driver.execute(args, () => new ValueMethodTestTop) {
		c => new ValueMethodTestTester(c)
    }

    println(chisel3.Driver.emitVerilog( new Top))

}