import Chisel._
import Chisel.iotesters.PeekPokeTester
import yellowspec._

class MultiValueConsumers extends Module with Yellowspec {

	val io = IO(new Bundle {
		val read = Flipped(ValueMethodIO(Void, UInt(3.W)))
	}
    )
	
	val outs = ValueMethodArbiter(io.read,3)


	val valid = Reg(Bool())
	val data = Reg(UInt(3.W))

	val cnt = Reg(UInt(10.W))

	val cycle = Counter(2)

	rule(cycle.value === 0.U){
		Chisel.printf("0:%d:%d\n", cnt, outs(0)(Void))
		cycle.inc()
	}

	rule( cycle.value === 1.U) {
		Chisel.printf("1:%d:%d\n", cnt, outs(1)(Void))
		cycle.inc()
	}
	rule() {
		Chisel.printf("2:%d:%d\n", cnt, outs(2)(Void))
    }


	rule() {
		cnt := cnt + 1.U
	}


}

class ValueMethodArbiterTestTop extends Module {

	val io = IO(new Bundle {})


	val p = Module(new ValueMethodProducer)
	val c = Module(new MultiValueConsumers)

	c.io.read <> p.io.read

	Flipped(Decoupled(0.U))
}

class ValueMethodArbiterTestTopUnitTester(c: ValueMethodArbiterTestTop) extends PeekPokeTester(c) {

	private val gcd = c

	for (i <- 1 to 40) {
		step(1)
	}
}


object ValueMethodArbiterTopTest extends App {


	chisel3.iotesters.Driver.execute(args, () => new ValueMethodArbiterTestTop) {
		c => new ValueMethodArbiterTestTopUnitTester(c)
	}
	//println(chisel3.Driver.emitVerilog( new Top))
}