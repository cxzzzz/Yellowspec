

import Chisel._
import Chisel.iotesters.PeekPokeTester
import yellowspec._

class MaybeProducer extends Module with YellowSpec{

	val io = IO(new Bundle{
		val read = ActionMethodIO(NoneParam,UInt(3.W))  // (param,return)
	})


	val fifoIO =  Wire(Decoupled(UInt(3.W)) )

	val fifo = Queue(fifoIO,30)


	io.read := ActionMethod( fifo )


	/*
	method( io.read )( fifo.valid ){
		(noneParam) => {
			fifo.deq()
		}

	}.default{
		fifo.nodeq()
	}
	 */

	val num = Reg(UInt(3.W))
	num :=  0.U


	rule( fifoIO.ready ) {

		when(  num > 5.U) {

			fifoIO.enq(num)
		}.otherwise{
			fifoIO.noenq
		}

		num := num+1.U

	}.default {
		fifoIO.noenq()
	}




}

class MaybeConsumer extends Module with YellowSpec {

	val io = IO(new Bundle{
		val read = Flipped( ActionMethodIO(NoneParam,UInt(3.W)))
	}
	)

	val valid = Reg(Bool())
	val data = Reg(UInt(3.W))

	val cnt = Reg(UInt(10.W))

	rule(){

		Chisel.printf("%d:%d\n", cnt, io.read.maybe(NoneParam).getOrElse(0.U))
	}

	rule(){
		cnt := cnt + 1.U
	}


}

class MaybeTop extends  Module{

	val io = IO( new Bundle{})


	val p = Module(new MaybeProducer)
	val c = Module(new MaybeConsumer)

	c.io.read <> p.io.read

}

class MaybeTopUnitTester(c: MaybeTop) extends PeekPokeTester(c) {

	private val gcd = c

	for(i <- 1 to 40 ) {
		step(1)
	}
}


object MaybeTopTest extends App{


	chisel3.iotesters.Driver.execute(args, () => new MaybeTop) {
		c => new MaybeTopUnitTester(c)
	}
}
