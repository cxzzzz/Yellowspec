import Chisel._
import Chisel.iotesters.PeekPokeTester
import yellowspec._

class Producer extends Module with YellowSpec{

    val io = IO(new Bundle{
      val read = ActionMethodIO(NoneParam,UInt(3.W))  // (param,return)
    })

    val fifoIO =  Wire(Decoupled(UInt(3.W)) )

    val fifo = Queue(fifoIO,30)


    method( io.read )( fifo.valid ){
      (noneParam) => {
        fifo.deq()
      }

    }.default{
      fifo.nodeq()
    }

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

class Consumer extends Module with YellowSpec {

  val io = IO(new Bundle{
    val read = Flipped( ActionMethodIO(NoneParam,UInt(3.W)))
  }
  )

    val valid = Reg(Bool())
    val data = Reg(UInt(3.W))

    val cnt = Reg(UInt(10.W))

    rule(){
            Chisel.printf("%d:%d\n", cnt,io.read(NoneParam))
    }

    rule(){
        cnt := cnt + 1.U
    }


}

class Top extends  Module{

  val io = IO( new Bundle{})


  val p = Module(new Producer)
  val c = Module(new Consumer)

   c.io.read <> p.io.read

    Flipped(Decoupled(0.U))
}

class TopUnitTester(c: Top) extends PeekPokeTester(c) {

  private val gcd = c

  for(i <- 1 to 40 ) {
    step(1)
  }
}


object TopTest extends App{


  chisel3.iotesters.Driver.execute(args, () => new Top) {
    c => new TopUnitTester(c)
  }
}
