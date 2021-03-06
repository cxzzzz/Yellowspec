

import Chisel._
import Chisel.iotesters.PeekPokeTester
import yellowspec._

class MaybeProducer extends Module with Yellowspec{

	val io = IO(new Bundle{
		val read = ActionMethodIO(VoidParam,UInt(3.W))  // (param,return)
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


	val readys = Wire(Bool())

	when( readys ) {

		val c = Wire(Bool())
		when ( num > 3.U) {
			c := true.B
		}.otherwise(
			c := false.B
		)

		readys := c

	}.otherwise{
		readys := false.B
	}

	readys := false.B

	val b = Wire(8.U)
	b := num + 1.U

	rule( fifoIO.ready ) {

		var c = 0

		when(  { c = c+1 ; println(c.toString());b > 5.U} ) {

			fifoIO.enq(num)
		}.otherwise{
			fifoIO.noenq
		}

		num := num+1.U

	}




}

class MaybeConsumer extends Module with Yellowspec {

	val io = IO(new Bundle{
		val read = Flipped( ActionMethodIO(VoidParam,UInt(3.W)))
	}
	)

	val valid = Reg(Bool())
	val data = Reg(UInt(3.W))

	val cnt = Reg(UInt(10.W))

	rule(){

		Chisel.printf("%d:%d\n", cnt, io.read.maybe(VoidParam).getOrElse(0.U))
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


	var e : () => Unit = null
	class A( b : =>Unit){
		b
	}
	class B( d: => Unit){
		d
		e = () => d
	}

	{
		//var c = 0
		new A( {
			var c = 0

			new B( {c = c + 1 ; println(c.toString) })
		})
	}

	{
		new A({
			var c = 0
			e()
		})
	}

	chisel3.iotesters.Driver.execute(args, () => new MaybeTop) {
		c => new MaybeTopUnitTester(c)
	}
/*	val a = chisel3.Driver.emit(() => new MaybeTop)
	println(a)

 */
}
