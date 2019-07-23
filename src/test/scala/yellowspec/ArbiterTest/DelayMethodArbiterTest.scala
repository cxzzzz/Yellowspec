import Chisel._
import Chisel.iotesters.PeekPokeTester
import yellowspec._

class DelayMethodProducer extends Module with Yellowspec {

  val io = IO(new Bundle {
    val read = DelayMethodIO[Void,Void,Void,UInt,ActionMethodIO[Void,UInt]](
      ActionMethodIO(Void, Void),
      ActionMethodIO(Void, UInt(3.W))
    )
  })

  val fifoIO = Wire(Decoupled(UInt(3.W)))

  val fifo = Queue(fifoIO, 30)

  {
    val cnt = Counter(7)
    val paramsT = WReg(
      MaybeIO(io.read.request.params),
      Maybe(io.read.request.params, false.B)
    )

    rule(){
        cnt.inc()
    }

    io.read.request := ActionMethod() {
      //method(io.read)(fifo.valid)(
      (params: io.read.request.paramsType) =>
        {
          paramsT := Maybe(params)

          cnt.value := 0.U

          Void
        }
    }


    io.read.response := ActionMethod(fifo.valid && paramsT().isValid && cnt.value > 3.U) {
      (params: Void) =>
        {

          fifo.deq()
        }

    }.default {
      fifo.nodeq()
    }
  }

  val num = Reg(UInt(3.W))
  num := 0.U

  rule(fifoIO.ready) {

    when(num > 5.U) {

      fifoIO.enq(num)
    }.otherwise {
      fifoIO.noenq
    }

    num := num + 1.U

  }.default {
    fifoIO.noenq()
  }

}

class DelayMethodConsumers extends Module with Yellowspec {

  val io = IO(new Bundle {
    val read = Flipped(DelayMethodIO[Void,Void,Void,UInt,ActionMethodIO[Void,UInt]](
      ActionMethodIO(Void, Void),
      ActionMethodIO(Void, UInt(3.W))
    ))
  })


  val valid = Reg(Bool())
  val data = Reg(UInt(3.W))

  val cnt = Reg(UInt(10.W))
  val cycle = Counter(2)

  val outs = DelayMethodArbiter(io.read,2)

  {
    
    rule(cycle.value === 0.U) {
        outs(0).request(Void)
        cycle.inc()
        Chisel.printf("0.start,%d\n",cnt)
    }

    rule(){
        Chisel.printf("0.end,%d:%d\n", cnt, outs(0).response(Void))
    }
  }

  {
    
    rule(cycle.value === 1.U) {
        outs(1).request(Void)
        cycle.inc()
        Chisel.printf("1.start,%d\n",cnt)
    }

    rule(){
        Chisel.printf("1.end,%d:%d\n", cnt, outs(1).response(Void))
    }
  }

  rule() {
    cnt := cnt + 1.U
  }

}

class DelayMethodTestTop extends Module {

  val io = IO(new Bundle {})

  val p = Module(new DelayMethodProducer)
  val c = Module(new DelayMethodConsumers)

  c.io.read <> p.io.read

}

class DelayMethodTopUnitTester(c: DelayMethodTestTop) extends PeekPokeTester(c) {

  private val gcd = c

  for (i <- 1 to 40) {
    step(1)
  }
}

object DelayMethodTopTest extends App {

  chisel3.iotesters.Driver.execute(args, () => new DelayMethodTestTop) { c =>
    new DelayMethodTopUnitTester(c)
  }
  //println(chisel3.Driver.emitVerilog(new Top))
}
