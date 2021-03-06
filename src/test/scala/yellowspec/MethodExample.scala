import Chisel._
import Chisel.iotesters.PeekPokeTester
import yellowspec._

class Producer extends Module with Yellowspec {

	val io = IO(new Bundle {
		val read = ActionMethodIO(Void , new Bundle{val data = UInt(3.W)}) // (param,return)
	})

	val fifoIO = Wire(Decoupled(UInt(3.W)))

	val fifo = Queue(fifoIO, 30)


	io.read := ActionMethod(fifo.valid){
	//method(io.read)(fifo.valid)(
		(params: io.read.paramsType) => {
			val values = Wire( io.read.values.cloneType )
			values.data := fifo.deq()
			values
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

class Consumer extends Module with Yellowspec {

	val io = IO(new Bundle {
		val read = Flipped(ActionMethodIO(Void, new Bundle{ val data = UInt(3.W)}))
	}
	)

	val valid = Reg(Bool())
	val data = Reg(UInt(3.W))

	val cnt = Reg(UInt(10.W))

	rule() {
		Chisel.printf("%d:%d\n", cnt, io.read(Void).data)
	}

	rule() {
		cnt := cnt + 1.U
	}


}

class Top extends Module {

	val io = IO(new Bundle {})


	val p = Module(new Producer)
	val c = Module(new Consumer)

	c.io.read <> p.io.read

	Flipped(Decoupled(0.U))
}

class TopUnitTester(c: Top) extends PeekPokeTester(c) {

	private val gcd = c

	for (i <- 1 to 40) {
		step(1)
	}
}


object TopTest extends App {


	chisel3.iotesters.Driver.execute(args, () => new Top) {
		c => new TopUnitTester(c)
	}
	println(chisel3.Driver.emitVerilog( new Top))
}