// See LICENSE.SiFive for license details.

package freechips.rocketchip.amba.simple

import Chisel._
import chisel3.util.IrrevocableIO
import freechips.rocketchip.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util._
import freechips.rocketchip.unittest._
import freechips.rocketchip.tilelink._

class SimpleXbar(
  arbitrationPolicy: TLArbiter.Policy = TLArbiter.roundRobin,
  maxFlightPerId:    Int = 7,
  awQueueDepth:      Int = 2)(implicit p: Parameters) extends LazyModule
{
  require (maxFlightPerId >= 1)
  require (awQueueDepth >= 1)

  val node = new SimpleNexusNode(
    masterFn  = { seq =>
      seq(0).copy(
        masters = (SimpleXbar.mapInputIds(seq) zip seq) flatMap { case (range, port) =>
          port.masters map { master => master.copy(id = master.id.shift(range.start)) }
        }
      )
    },
    slaveFn = { seq =>
      seq(0).copy(
        minLatency = seq.map(_.minLatency).min,
        slaves = seq.flatMap { port =>
          require (port.beatBytes == seq(0).beatBytes,
            s"Xbar data widths don't match: ${port.slaves.map(_.name)} has ${port.beatBytes}B vs ${seq(0).slaves.map(_.name)} has ${seq(0).beatBytes}B")
          port.slaves
        }
      )
    }
  ){
    override def circuitIdentity = outputs == 1 && inputs == 1
  }

  lazy val module = new LazyModuleImp(this) {
    val (io_in, edgesIn) = node.in.unzip
    val (io_out, edgesOut) = node.out.unzip

    // Grab the port ID mapping
    val inputIdRanges = SimpleXbar.mapInputIds(edgesIn.map(_.master))

    // Find a good mask for address decoding
    val port_addrs = edgesOut.map(_.slave.slaves.map(_.address).flatten)
    val routingMask = AddressDecoder(port_addrs)
    val route_addrs = port_addrs.map(seq => AddressSet.unify(seq.map(_.widen(~routingMask)).distinct))
    val outputPorts = route_addrs.map(seq => (addr: UInt) => seq.map(_.contains(addr)).reduce(_ || _))

    // To route W we need to record where the AWs went
    val awIn  = Seq.fill(io_in.size) { Module(new Queue(UInt(width = io_out.size), awQueueDepth, flow = true)) }
    val awOut = Seq.fill(io_out.size) { Module(new Queue(UInt(width = io_in.size), awQueueDepth, flow = true)) }
    val requestAWIO = io_in.map  { i => Vec(outputPorts.map { o => o(i.req.bits.addr) }) }
    val requestBOI  = io_out.map { o => inputIdRanges.map { i => i.contains(o.rsp.bits.id) } }

    // W follows the path dictated by the AW Q
    for (i <- 0 until io_in.size) { awIn(i).io.enq.bits := requestAWIO(i).asUInt }

    // We need an intermediate size of bundle with the widest possible identifiers
    val wide_bundle = SimpleBundleParameters.union(io_in.map(_.params) ++ io_out.map(_.params))

    // Transform input bundles
    val in = Wire(Vec(io_in.size, SimpleBundle(wide_bundle)))
    for (i <- 0 until in.size) {
      in(i) :<> io_in(i)

      // Handle size = 1 gracefully (Chisel3 empty range is broken)
      def trim(id: UInt, size: Int) = if (size <= 1) UInt(0) else id(log2Ceil(size)-1, 0)
      // Manipulate the AXI IDs to differentiate masters
      val r = inputIdRanges(i)
      in(i).req.bits.id := io_in(i).req.bits.id | UInt(r.start)
      io_in(i).rsp.bits.id := trim(in(i).rsp.bits.id, r.size)

      if (io_out.size > 1) {
        // Block A[RW] if we switch ports, to ensure responses stay ordered (also: beware the dining philosophers)
        val endId = edgesIn(i).master.endId
        val arFIFOMap = Wire(init = Vec.fill(endId) { Bool(true) })
        val awFIFOMap = Wire(init = Vec.fill(endId) { Bool(true) })
        val awSel = UIntToOH(io_in(i).req.bits.id, endId)
        val bSel  = UIntToOH(io_in(i).rsp .bits.id, endId)
        val awTag = OHToUInt(requestAWIO(i).asUInt, io_out.size)

        for (master <- edgesIn(i).master.masters) {
          def idTracker(port: UInt, req_fire: Bool, resp_fire: Bool) = {
            if (master.maxFlight == Some(0)) {
              Bool(true)
            } else {
              val legalFlight = master.maxFlight.getOrElse(maxFlightPerId+1)
              val flight = legalFlight min maxFlightPerId
              val canOverflow = legalFlight > flight
              val count = RegInit(UInt(0, width = log2Ceil(flight+1)))
              val last = Reg(UInt(width = log2Ceil(io_out.size)))
              count := count + req_fire.asUInt - resp_fire.asUInt
              assert (!resp_fire || count =/= UInt(0))
              assert (!req_fire  || count =/= UInt(flight))
              when (req_fire) { last := port }
              // No need to track where it went if we cap it at 1 request
              val portMatch = if (flight == 1) { Bool(true) } else { last === port }
              (count === UInt(0) || portMatch) && (Bool(!canOverflow) || count =/= UInt(flight))
            }
          }

          for (id <- master.id.start until master.id.end) {
            awFIFOMap(id) := idTracker(
              awTag,
              awSel(id) && io_in(i).req.fire(),
              bSel(id) && io_in(i).rsp.fire())
          }
        }

        // Keep in mind that slaves may do this: awready := wvalid, wready := awvalid
        // To not cause a loop, we cannot have: wvalid := awready

        // Block AW if we cannot record the W destination
        val allowAW = awFIFOMap(io_in(i).req.bits.id)
        val latched = RegInit(Bool(false)) // cut awIn(i).enq.valid from awready
        in(i).req.valid := io_in(i).req.valid && (latched || awIn(i).io.enq.ready) && allowAW
        io_in(i).req.ready := in(i).req.ready && (latched || awIn(i).io.enq.ready) && allowAW
        awIn(i).io.enq.valid := io_in(i).req.valid && !latched
        when (awIn(i).io.enq.fire()) { latched := Bool(true) }
        when (in(i).req.fire()) { latched := Bool(false) }
      }
    }

    // Transform output bundles
    val out = Wire(Vec(io_out.size, SimpleBundle(wide_bundle)))
    for (i <- 0 until out.size) {
      io_out(i) :<> out(i)

      if (io_in.size > 1) {
        // Block AW if we cannot record the W source
        val latched = RegInit(Bool(false)) // cut awOut(i).enq.valid from awready
        io_out(i).req.valid := out(i).req.valid && (latched || awOut(i).io.enq.ready)
        out(i).req.ready := io_out(i).req.ready && (latched || awOut(i).io.enq.ready)
        awOut(i).io.enq.valid := out(i).req.valid && !latched
        when (awOut(i).io.enq.fire()) { latched := Bool(true) }
        when (out(i).req.fire()) { latched := Bool(false) }
      }
    }

    // Fanout the input sources to the output sinks
    def transpose[T](x: Seq[Seq[T]]) = Seq.tabulate(x(0).size) { i => Seq.tabulate(x.size) { j => x(j)(i) } }
    val portsAWOI = transpose((in  zip requestAWIO) map { case (i, r) => SimpleXbar.fanout(i.req, r) })
    val portsBIO  = transpose((out zip requestBOI)  map { case (o, r) => SimpleXbar.fanout(o.rsp,  r) })

    // Arbitrate amongst the sources
    for (o <- 0 until out.size) {
      awOut(o).io.enq.bits := // Record who won AW arbitration to select W
        SimpleArbiter.returnWinner(arbitrationPolicy)(out(o).req, portsAWOI(o):_*).asUInt
    }

    for (i <- 0 until in.size) {
      SimpleArbiter(arbitrationPolicy)(in(i).rsp, portsBIO(i):_*)
    }
  }
}

object SimpleXbar
{
  def apply(
    arbitrationPolicy: TLArbiter.Policy = TLArbiter.roundRobin,
    maxFlightPerId:    Int = 7,
    awQueueDepth:      Int = 2)(implicit p: Parameters) =
  {
    val sramqxbar = LazyModule(new SimpleXbar(arbitrationPolicy, maxFlightPerId, awQueueDepth))
    sramqxbar.node
  }

  def mapInputIds(ports: Seq[SimpleMasterPortParameters]) = TLXbar.assignRanges(ports.map(_.endId))

  // Replicate an input port to each output port
  def fanout[T <: SimpleBundleBase](input: IrrevocableIO[T], select: Seq[Bool]) = {
    val filtered = Wire(Vec(select.size, input))
    for (i <- 0 until select.size) {
      filtered(i).bits :<= input.bits
      filtered(i).valid := input.valid && select(i)
    }
    input.ready := Mux1H(select, filtered.map(_.ready))
    filtered
  }
}

object SimpleArbiter
{
  def apply[T <: Data](policy: TLArbiter.Policy)(sink: IrrevocableIO[T], sources: IrrevocableIO[T]*): Unit = {
    if (sources.isEmpty) {
      sink.valid := Bool(false)
    } else {
      returnWinner(policy)(sink, sources:_*)
    }
  }
  def returnWinner[T <: Data](policy: TLArbiter.Policy)(sink: IrrevocableIO[T], sources: IrrevocableIO[T]*) = {
    require (!sources.isEmpty)

    // The arbiter is irrevocable; when !idle, repeat last request
    val idle = RegInit(Bool(true))

    // Who wants access to the sink?
    val valids = sources.map(_.valid)
    val anyValid = valids.reduce(_ || _)
    // Arbitrate amongst the requests
    val readys = Vec(policy(valids.size, Cat(valids.reverse), idle).asBools)
    // Which request wins arbitration?
    val winner = Vec((readys zip valids) map { case (r,v) => r&&v })

    // Confirm the policy works properly
    require (readys.size == valids.size)
    // Never two winners
    val prefixOR = winner.scanLeft(Bool(false))(_||_).init
    assert((prefixOR zip winner) map { case (p,w) => !p || !w } reduce {_ && _})
    // If there was any request, there is a winner
    assert (!anyValid || winner.reduce(_||_))

    // The one-hot source granted access in the previous cycle
    val state = RegInit(Vec.fill(sources.size)(Bool(false)))
    val muxState = Mux(idle, winner, state)
    state := muxState

    // Determine when we go idle
    when (anyValid) { idle := Bool(false) }
    when (sink.fire()) { idle := Bool(true) }

    if (sources.size > 1) {
      val allowed = Mux(idle, readys, state)
      (sources zip allowed) foreach { case (s, r) =>
        s.ready := sink.ready && r
      }
    } else {
      sources(0).ready := sink.ready
    }

    sink.valid := Mux(idle, anyValid, Mux1H(state, valids))
    sink.bits :<= Mux1H(muxState, sources.map(_.bits))
    muxState
  }
}

class SimpleXbarFuzzTest(name: String, txns: Int, nMasters: Int, nSlaves: Int)(implicit p: Parameters) extends LazyModule with BindingScope
{
  lazy val json = JSON(bindingTree)

  ElaborationArtefacts.add(s"${name}.graphml", graphML)
  //ElaborationArtefacts.add(s"${name}.dts", outer.dts)
  ElaborationArtefacts.add(s"${name}.json", json)

  val xbar = SimpleXbar()
  val slaveSize = 0x1000
  val masterBandSize = slaveSize >> log2Ceil(nMasters)
  def filter(i: Int) = TLFilter.mSelectIntersect(AddressSet(i * masterBandSize, ~BigInt(slaveSize - masterBandSize)))

  val slaves = Seq.tabulate(nSlaves) { i => LazyModule(new SimpleRAM(AddressSet(slaveSize * i, slaveSize-1))) }
  slaves.foreach { s => (s.node
    //:= SimpleFragmenter()
    := SimpleBuffer(BufferParams.flow)
    := SimpleBuffer(BufferParams.flow)
    //:= SimpleDelayer(0.25)
    := xbar) }

  val masters = Seq.fill(nMasters) { LazyModule(new TLFuzzer(txns, 4, nOrdered = Some(1))) }
  masters.zipWithIndex.foreach { case (m, i) => (xbar
    //:= SimpleDelayer(0.25)
    //:= SimpleDeinterleaver(4096)
    := TLToSimple()
    := TLFilter(filter(i))
    := TLRAMModel(s"${name} Master $i")
    := m.node) }

  lazy val module = new LazyModuleImp(this) with UnitTestModule {
    io.finished := masters.map(_.module.io.finished).reduce(_ || _)
  }
}

class SimpleXbarTest(txns: Int = 5000, timeout: Int = 500000)(implicit p: Parameters) extends UnitTest(timeout) {
  //val dut21 = Module(LazyModule(new SimpleXbarFuzzTest("Xbar DUT21", txns, 2, 1)).module)
  //val dut12 = Module(LazyModule(new SimpleXbarFuzzTest("Xbar DUT12", txns, 1, 2)).module)
  val dut22 = Module(LazyModule(new SimpleXbarFuzzTest("Xbar DUT22", txns, 2, 4)).module)
  io.finished := Seq(/*dut21, dut12, */dut22).map(_.io.finished).reduce(_ || _)
}


