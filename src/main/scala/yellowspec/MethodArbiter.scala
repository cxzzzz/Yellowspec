package yellowspec

import chisel3._
import chisel3.util.log2Ceil

class AtomicMethodArbiterIO[PT <: Data, VT <: Data, MT <: AtomicMethodIO[
  PT,
  VT
]](methodIOGen: MT, n: Int)
    extends Bundle {

  val in = Flipped(methodIOGen)
  val out = Vec(n, methodIOGen)
  //val chosen = Output(UInt(log2Ceil(n).W))
}

/** Arbiter Control determining which producer has access
  */
private object ArbiterCtrl {
  def apply(request: Seq[Bool]): Seq[Bool] = request.length match {
    case 0 => Seq()
    case 1 => Seq(true.B)
    case _ => true.B +: request.tail.init.scanLeft(request.head)(_ || _).map(!_)
  }
}

abstract class ArbiterLike[PT <: Data, VT <: Data, MT <: AtomicMethodIO[PT, VT]](
    methodIOGen: MT,
    n: Int,
    count: Int,
    needsLock: Option[MT => Bool]
) extends Module {
  protected def grant: Seq[Bool]
  protected def choice: UInt
  protected def connect(grant: Seq[Bool], choice: UInt): Unit // signal connect

  val io: MT

  connect(grant, choice)
}

class ActionMethodArbiter[PT <: Data, VT <: Data](
    methodIOGen: ActionMethodIO[PT, VT],
    n: Int
) extends Module {

  val io = IO(
    new AtomicMethodArbiterIO[PT, VT, ActionMethodIO[PT, VT]](methodIOGen, n)
  )

  for (i <- 0 until n) {
    io.out(i).values := io.in.values
  }

  io.in.params := io.out(n - 1).params

  for (i <- n - 2 to 0 by -1) {
    when(io.out(i).valid) {
      io.in.params := io.out(i).params
    }
  }

  private val grant = ArbiterCtrl(io.out.map(_.ready))

  for ((out, g) <- (io.out zip grant))
    out.valid := g && io.in.valid

  io.in.ready := !grant.last || io.out.last.ready

}

// valuemethodarbiter can't support valuemethod who's param isn't Void
// now , because of lacking of ready signal

class ValueMethodArbiter[VT <: Data](
    methodIOGen: ValueMethodIO[VoidType.Void, VT],
    n: Int
) extends Module {

  require(
    VoidType.isVoid(methodIOGen.params),
    "ValueMethod's param must be Void now , for lacking of ready signal"
  )

  val io = IO(
    new AtomicMethodArbiterIO[
      VoidType.Void,
      VT,
      ValueMethodIO[VoidType.Void, VT]
    ](methodIOGen, n)
  )

  for (i <- 0 until n) {
    io.out(i).values := io.in.values
    io.out(i).valid := io.in.valid
  }

  io.in.params := io.out(n - 1).params

}

class DelayMethodArbiter[
    APT <: Data,
    AVT <: Data,
    RPT <: Data,
    RVT <: Data,
    RMT <: AtomicMethodIO[RPT, RVT]
](methodIOGen: DelayMethodIO[APT, AVT, RPT, RVT, RMT], n: Int)
    extends Module {

  val io = IO(
    new Bundle {
      val in = Flipped(methodIOGen)
      val out = Vec(n,methodIOGen)
    }
  )

  private val grant = ArbiterCtrl(io.out.map(_.request.ready))

  private val reqOutReady = !grant.last || io.out(n - 1).request.ready

  val lock = WReg(Bool(), false.B)
  val lockCnt = WReg(UInt())

  val start = lock.w && (!lock.r)

  val requestFire = io.in.response.isValid && io.out(lockCnt.w).response.isReady
  val responseStartFire = io.in.request.isValid && io
    .out(lockCnt.w)
    .request
    .isReady
  val responseOtherFire = io.in.request.isValid && io
    .out(lockCnt.r)
    .request
    .isReady

  //connect io.out and io.in signals(except ready and valid signal)
  for (i <- 0 until n) {
    io.out(i) := io.in
    io.out(i).request.valid := false.B
    io.out(i).response.valid := false.B
  }

  io.in.noen()

  //lock.w and lock.r
  when(!lock.r) { // arbiter not running
    when(requestFire && !responseStartFire) {
      lock := true.B
    }.otherwise {
      lock := false.B
    }
  }.elsewhen(responseOtherFire) { //arbiter is running
    lock := false.B
  }

  //select out , lower index preferred
  for ((out, i) <- (io.out.zipWithIndex).reverse) {
    when(out.request.ready) {
      lockCnt.w := i.U
    }
  }

  when(lock.w || lock.r) { // arbiter is running
    when(!lock.r) // start
    {
      io.out(lockCnt.w) := io.in
      lockCnt.r := lockCnt.w
    }.otherwise {
      io.out(lockCnt.r).response := io.in.response
    }
  }
}

object ActionMethodArbiter {

  def apply[PT <: Data, VT <: Data](
      methodIO: ActionMethodIO[PT, VT],
      n: Int
  ): Vec[ActionMethodIO[PT, VT]] = {
    val arbiter = Module(new ActionMethodArbiter(methodIO.cloneType, n))
    arbiter.io.in := methodIO
    arbiter.io.out
  }

}

object ValueMethodArbiter {

  def apply[VT <: Data](
      methodIO: ValueMethodIO[VoidType.Void, VT],
      n: Int
  ): Vec[ValueMethodIO[VoidType.Void, VT]] = {
    val arbiter = Module(new ValueMethodArbiter(methodIO.cloneType, n))
    arbiter.io.in := methodIO
    arbiter.io.out
  }

}

object DelayMethodArbiter {
  def apply[
      APT <: Data,
      AVT <: Data,
      RPT <: Data,
      RVT <: Data,
      RMT <: AtomicMethodIO[RPT, RVT]
  ](
      methodIO: DelayMethodIO[APT, AVT, RPT, RVT, RMT],
      n: Int
  ): Vec[DelayMethodIO[APT, AVT, RPT, RVT, RMT]] = {
    val arbiter = Module(new DelayMethodArbiter(methodIO.cloneType, n))
    arbiter.io.in := methodIO
    arbiter.io.out
  }
}
