// See LICENSE.SiFive for license details.

package freechips.rocketchip.amba.custom

/*import chisel3._
import chisel3.util._
import freechips.rocketchip.config._
import freechips.rocketchip.util._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._

class CustomXbar(beatBytes: Int, policy: TLArbiter.Policy = TLArbiter.roundRobin)(implicit p: Parameters) extends LazyModule
{
  val node = CustomNexusNode(
    masterFn  = { seq =>
      seq.foreach { port => require(port.userFields == seq(0).userFields) }
      seq(0).v1copy(
        beatBytes = Some(beatBytes),
        masters = (CustomXbar.mapInputIds(seq) zip seq) flatMap { case (range, port) =>
          port.masters map { master => master.v1copy(sourceId = master.sourceId.shift(range.start))}})},
    slaveFn = { seq =>
      seq(0).v1copy(
        beatBytes = Some(beatBytes),
        slaves = (CustomXbar.mapOutputIds(seq) zip seq) flatMap { case (range, port) =>
          port.slaves.map { slave => slave.v1copy(destinationId = slave.destinationId + range.start)}})})

  lazy val module = new LazyModuleImp(this) {
    val (io_in, edgesIn) = node.in.unzip
    val (io_out, edgesOut) = node.out.unzip

    // Grab the port ID mapping
    val inputIdRanges = CustomXbar.mapInputIds(edgesIn.map(_.master))
    val outputIdRanges = CustomXbar.mapOutputIds(edgesOut.map(_.slave))

    // We need an intermediate size of bundle with the widest possible identifiers
    val wide_bundle = CustomBundleParameters.union(io_in.map(_.params) ++ io_out.map(_.params))

    // Handle size = 1 gracefully (Chisel3 empty range is broken)
    def trim(id: UInt, size: Int) = if (size <= 1) 0.U else id(log2Ceil(size)-1, 0)

    // Transform input bundle sources (dest uses global namespace on both sides)
    val in = Wire(Vec(io_in.size, CustomBundle(wide_bundle)))
    for (i <- 0 until in.size) {
      in(i) :<> io_in(i)
      in(i).bits.lift(CustomId) foreach { _ := io_in(i).bits.id | inputIdRanges(i).start.U }
    }

    // Transform output bundle sinks (id use global namespace on both sides)
    val out = Wire(Vec(io_out.size, CustomBundle(wide_bundle)))
    for (o <- 0 until out.size) {
      io_out(o) :<> out(o)
      io_out(o).bits.lift(CustomDest) foreach { _ := trim(out(o).bits.dest, outputIdRanges(o).size) }
    }

    // Fanout the input sources to the output sinks
    val request = in.map { i => outputIdRanges.map { o => o.contains(i.bits.dest) } }
    val ports = (in zip request) map { case (i, r) => CustomXbar.fanout(i, r) }

    // Arbitrate amongst the sources
    (out zip ports.transpose) map { case (o, i) => CustomXbar.arbitrate(policy)(o, i) }
  }
}

object CustomXbar
{
  def apply(beatBytes: Int, policy: TLArbiter.Policy = TLArbiter.roundRobin)(implicit p: Parameters): CustomNode =
  {
    val xbar = LazyModule(new CustomXbar(beatBytes, policy))
    xbar.node
  }

  def mapInputIds (ports: Seq[CustomMasterPortParameters]) = TLXbar.assignRanges(ports.map(_.endSourceId))
  def mapOutputIds(ports: Seq[CustomSlavePortParameters]) = TLXbar.assignRanges(ports.map(_.endDestinationId))

  def arbitrate(policy: TLArbiter.Policy)(sink: CustomBundle, sources: Seq[CustomBundle]): Unit = {
    if (sources.isEmpty) {
      sink.valid := false.B
    } else if (sources.size == 1) {
      sink :<> sources.head
    } else {
      // The number of beats which remain to be sent
      val idle = RegInit(true.B)
      when (sink.valid) { idle := sink.bits.last && sink.ready }

      // Who wants access to the sink?
      val valids = sources.map(_.valid)
      // Arbitrate amongst the requests
      val readys = VecInit(policy(valids.size, Cat(valids.reverse), idle).asBools)
      // Which request wins arbitration?
      val winner = VecInit((readys zip valids) map { case (r,v) => r&&v })

      // Confirm the policy works properly
      require (readys.size == valids.size)
      // Never two winners
      val prefixOR = winner.scanLeft(false.B)(_||_).init
      assert((prefixOR zip winner) map { case (p,w) => !p || !w } reduce {_ && _})
      // If there was any request, there is a winner
      assert (!valids.reduce(_||_) || winner.reduce(_||_))

      // The one-hot source granted access in the previous cycle
      val state = RegInit(VecInit.tabulate(sources.size)(_ => false.B))
      val muxState = Mux(idle, winner, state)
      state := muxState

      val allowed = Mux(idle, readys, state)
      (sources zip allowed) foreach { case (s, r) => s.ready := sink.ready && r }
      sink.valid := Mux(idle, valids.reduce(_||_), Mux1H(state, valids))
      sink.bits :<= Mux1H(muxState, sources.map(_.bits))
    }
  }

  def fanout(input: CustomBundle, select: Seq[Bool]): Seq[CustomBundle] = {
    val filtered = Wire(Vec(select.size, chiselTypeOf(input)))
    for (i <- 0 until select.size) {
      filtered(i).bits :<= input.bits
      filtered(i).valid := input.valid && (select(i) || (select.size == 1).B)
    }
    input.ready := Mux1H(select, filtered.map(_.ready))
    filtered
  }
}

*/


