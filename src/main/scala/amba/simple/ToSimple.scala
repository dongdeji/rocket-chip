// See LICENSE.SiFive for license details.

package freechips.rocketchip.tilelink

import Chisel._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util._
import freechips.rocketchip.amba.simple._
import freechips.rocketchip.amba.simple._
import freechips.rocketchip.amba._

class SimpleTLStateBundle(val sourceBits: Int) extends Bundle {
  val size   = UInt(width = 4)
  val source = UInt(width = sourceBits max 1)
}

case object SimpleTLState extends ControlKey[SimpleTLStateBundle]("tl_state")
case class SimpleTLStateField(sourceBits: Int) extends BundleField(SimpleTLState) {
  def data = Output(new SimpleTLStateBundle(sourceBits))
  def default(x: SimpleTLStateBundle) = {
    x.size   := 0.U
    x.source := 0.U
  }
}

/** TLtoSimpleIdMap serves as a record for the translation performed between id spaces.
  *
  * Its member [sramqMasters] is used as the new SimpleMasterParameters in diplomacy.
  * Its member [mapping] is used as the template for the circuit generated in TLToSimpleNode.module.
  */
class TLtoSimpleIdMap(tlPort: TLMasterPortParameters) extends IdMap[TLToSimpleIdMapEntry]
{
  val tlMasters = tlPort.masters.sortBy(_.sourceId).sortWith(TLToSimple.sortByType)
  private val sramqIdSize = tlMasters.map { tl => if (tl.requestFifo) 1 else tl.sourceId.size }
  private val sramqIdStart = sramqIdSize.scanLeft(0)(_+_).init
  val sramqMasters = sramqIdStart.zip(sramqIdSize).zip(tlMasters).map { case ((start, size), tl) =>
    SimpleMasterParameters(
      name      = tl.name,
      id        = IdRange(start, start+size),
      //by dongdeji aligned   = true,
      maxFlight = Some(if (tl.requestFifo) tl.sourceId.size else 1),
      nodePath  = tl.nodePath)
  }

  private val sramqIdEnd = sramqMasters.map(_.id.end).max
  private val axiDigits = String.valueOf(sramqIdEnd-1).length()
  private val tlDigits = String.valueOf(tlPort.endSourceId-1).length()
  protected val fmt = s"\t[%${axiDigits}d, %${axiDigits}d) <= [%${tlDigits}d, %${tlDigits}d) %s%s%s"

  val mapping: Seq[TLToSimpleIdMapEntry] = tlMasters.zip(sramqMasters).map { case (tl, axi) =>
    TLToSimpleIdMapEntry(axi.id, tl.sourceId, tl.name, tl.supports.probe, tl.requestFifo)
  }
}

case class TLToSimpleIdMapEntry(sramqId: IdRange, tlId: IdRange, name: String, isCache: Boolean, requestFifo: Boolean)
  extends IdMapEntry
{
  val from = tlId
  val to = sramqId
  val maxTransactionsInFlight = Some(tlId.size)
}

case class TLToSimpleNode(wcorrupt: Boolean = true)(implicit valName: ValName) extends MixedAdapterNode(TLImp, SimpleImp)(
  dFn = { p =>
    SimpleMasterPortParameters(
      masters    = (new TLtoSimpleIdMap(p)).sramqMasters)
  },
  uFn = { p => TLSlavePortParameters.v1(
    managers = p.slaves.map { case s =>
      TLSlaveParameters.v1(
        address            = s.address,
        resources          = s.resources,
        nodePath           = s.nodePath,
        supportsGet        = s.supportsRead,
        supportsPutFull    = s.supportsWrite,
        supportsPutPartial = s.supportsWrite,
        fifoId             = Some(0),
        mayDenyPut         = true,
        mayDenyGet         = true)},
      beatBytes = p.beatBytes,
      minLatency = p.minLatency)
  })

// wcorrupt alone is not enough; a slave must include AMBACorrupt in the slave port's requestKeys
class TLToSimple(val combinational: Boolean = true, val adapterName: Option[String] = None, val stripBits: Int = 0, val wcorrupt: Boolean = true)(implicit p: Parameters) extends LazyModule
{
  require(stripBits == 0, "stripBits > 0 is no longer supported on TLToSimple")
  val node = TLToSimpleNode(wcorrupt)

  lazy val module = new LazyModuleImp(this) {
    (node.in zip node.out) foreach { case ((in, edgeIn), (out, edgeOut)) =>
      val slaves  = edgeOut.slave.slaves

      // All pairs of slaves must promise that they will never interleave data

      // Construct the source=>ID mapping table
      val map = new TLtoSimpleIdMap(edgeIn.client)
      val sourceStall = Wire(Vec(edgeIn.client.endSourceId, Bool()))
      val sourceTable = Wire(Vec(edgeIn.client.endSourceId, out.req.bits.id))
      val idStall = Wire(init = Vec.fill(edgeOut.master.endId) { Bool(false) })
      var idCount = Array.fill(edgeOut.master.endId) { None:Option[Int] }

      map.mapping.foreach { case TLToSimpleIdMapEntry(sramqId, tlId, _, _, fifo) =>
        for (i <- 0 until tlId.size) {
          val id = sramqId.start + (if (fifo) 0 else i)
          sourceStall(tlId.start + i) := idStall(id)
          sourceTable(tlId.start + i) := UInt(id)
        }
        if (fifo) { idCount(sramqId.start) = Some(tlId.size) }
      }

      adapterName.foreach { n =>
        println(s"$n Simple-ID <= TL-Source mapping:\n${map.pretty}\n")
        ElaborationArtefacts.add(s"$n.simple.json", s"""{"mapping":[${map.mapping.mkString(",")}]}""")
      }

      // We need to keep the following state from A => D: (size, source)
      // All of those fields could potentially require 0 bits (argh. Chisel.)
      // We will pack all of that extra information into the echo bits.

      require (log2Ceil(edgeIn.maxLgSize+1) <= 4)
      val a_address = edgeIn.address(in.a.bits)
      val a_source  = in.a.bits.source
      val a_size    = edgeIn.size(in.a.bits)
      val a_isPut   = edgeIn.hasData(in.a.bits)
      val (a_first, a_last, _) = edgeIn.firstlast(in.a)

      //by dongdeji val r_state = out.r.bits.echo(SimpleTLState)
      //by dongdeji val r_source  = r_state.source
      //by dongdeji val r_size    = r_state.size

      //by dongdeji val b_state = out.rsp.bits.echo(SimpleTLState)
      //by dongdeji val b_source  = b_state.source
      //by dongdeji val b_size    = b_state.size

      // We need these Queues because Simple queues are irrevocable
      val depth = if (combinational) 1 else 2
      val out_arw = Wire(Decoupled(new SimpleReqBundle(out.params)))
      //by dongdeji val out_w = Wire(out.w)
      //by dongdeji out.w :<> Queue.irrevocable(out_w, entries=depth, flow=combinational)
      val queue_arw = Queue.irrevocable(out_arw, entries=depth, flow=combinational)

      // Fan out the ARW channel to AR and AW
      //by dongdeji out.ar.bits := queue_arw.bits
      out.req.bits := queue_arw.bits
      //by dongdeji out.ar.valid := queue_arw.valid && !queue_arw.bits.wen
      out.req.valid := queue_arw.valid //by dongdeji &&  queue_arw.bits.wen
      //by dongdeji queue_arw.ready := Mux(queue_arw.bits.wen, out.aw.ready, out.ar.ready)
      queue_arw.ready := out.req.ready

      val beatBytes = edgeIn.manager.beatBytes
      val maxSize   = UInt(log2Ceil(beatBytes))
      val doneAW    = RegInit(Bool(false))
      when (in.a.fire()) { doneAW := !a_last }

      val arw = out_arw.bits
      //by dongdeji arw.wen   := a_isPut
      arw.id    := sourceTable(a_source)
      arw.addr  := a_address
      //by dongdeji arw.len   := UIntToOH1(a_size, SimpleParameters.lenBits + log2Ceil(beatBytes)) >> log2Ceil(beatBytes)
      //by dongdeji arw.size  := Mux(a_size >= maxSize, maxSize, a_size)
      //by dongdeji arw.burst := SimpleParameters.BURST_INCR
      //by dongdeji arw.lock  := UInt(0) // not exclusive (LR/SC unsupported b/c no forward progress guarantee)
      //by dongdeji arw.cache := UInt(0) // do not allow AXI to modify our transactions
      //by dongdeji arw.prot  := SimpleParameters.PROT_PRIVILEDGED
      //by dongdeji arw.qos   := UInt(0) // no QoS
      //by dongdeji arw.user :<= in.a.bits.user
      //by dongdeji arw.echo :<= in.a.bits.echo
      //by dongdeji val a_extra = arw.echo(SimpleTLState)
      //by dongdeji a_extra.source := a_source
      //by dongdeji a_extra.size   := a_size

      in.a.bits.user.lift(AMBAProt).foreach { x =>
        val prot  = Wire(Vec(3, Bool()))
        val cache = Wire(Vec(4, Bool()))
        prot(0) :=  x.privileged
        prot(1) := !x.secure
        prot(2) :=  x.fetch
        cache(0) := x.bufferable
        cache(1) := x.modifiable
        cache(2) := x.readalloc
        cache(3) := x.writealloc
        //by dongdeji arw.prot  := Cat(prot.reverse)
        //by dongdeji arw.cache := Cat(cache.reverse)
      }

      val stall = sourceStall(in.a.bits.source) && a_first
      in.a.ready := !stall && Mux(a_isPut, (doneAW || out_arw.ready)/* && out_w.ready*/, out_arw.ready)
      out_arw.valid := !stall && in.a.valid && Mux(a_isPut, !doneAW/* && out_w.ready*/, Bool(true))

      //by dongdeji out_w.valid := !stall && in.a.valid && a_isPut && (doneAW || out_arw.ready)
      //by dongdeji out_w.bits.data := in.a.bits.data
      //by dongdeji out_w.bits.strb := in.a.bits.mask
      //by dongdeji out_w.bits.last := a_last
      //by dongdeji out_w.bits.user.lift(AMBACorrupt).foreach { _ := in.a.bits.corrupt }

      // R and B => D arbitration
      val r_holds_d = RegInit(Bool(false))
      //by dongdeji when (out.r.fire()) { r_holds_d := !out.r.bits.last }
      // Give R higher priority than B, unless B has been delayed for 8 cycles
      val b_delay = Reg(UInt(width=3))
      when (out.rsp.valid && !out.rsp.ready) {
        b_delay := b_delay + UInt(1)
      } .otherwise {
        b_delay := UInt(0)
      }
      //by dongdeji val r_wins = (out.r.valid && b_delay =/= UInt(7)) || r_holds_d

      //by dongdeji out.r.ready := in.d.ready && r_wins
      out.rsp.ready := in.d.ready //by dongdeji && !r_wins
      //by dongdeji in.d.valid := Mux(r_wins, out.r.valid, out.rsp.valid)

      // If the first beat of the AXI RRESP is RESP_DECERR, treat this as a denied
      // request. We must pulse extend this value as AXI is allowed to change the
      // value of RRESP on every beat, and ChipLink may not.
      val r_first = RegInit(Bool(true))
      //by dongdeji when (out.r.fire()) { r_first := out.r.bits.last }
      //by dongdeji val r_denied  = out.r.bits.resp === SimpleParameters.RESP_DECERR holdUnless r_first
      //by dongdeji val r_corrupt = out.r.bits.resp =/= SimpleParameters.RESP_OKAY
      //by dongdeji val b_denied  = out.rsp.bits.resp =/= SimpleParameters.RESP_OKAY

      //by dongdeji val r_d = edgeIn.AccessAck(r_source, r_size, UInt(0), denied = r_denied, corrupt = r_corrupt || r_denied)
      //by dongdeji val b_d = edgeIn.AccessAck(b_source, b_size, denied = b_denied)
      //by dongdeji r_d.user :<= out.r.bits.user
      //by dongdeji r_d.echo :<= out.r.bits.echo
      //by dongdeji b_d.user :<= out.rsp.bits.user
      //by dongdeji b_d.echo :<= out.rsp.bits.echo

      //by dongdeji in.d.bits := Mux(r_wins, r_d, b_d)
      //by dongdeji in.d.bits := b_d
      in.d.bits.data := out.rsp.bits.data // avoid a costly Mux

      // We need to track if any reads or writes are inflight for a given ID.
      // If the opposite type arrives, we must stall until it completes.
      val a_sel = UIntToOH(arw.id, edgeOut.master.endId).asBools
      val d_sel = UIntToOH(out.rsp.bits.id, edgeOut.master.endId).asBools
      val d_last = Bool(true)
      // If FIFO was requested, ensure that R+W ordering is preserved
      (a_sel zip d_sel zip idStall zip idCount) foreach { case (((as, ds), s), n) =>
        // AXI does not guarantee read vs. write ordering. In particular, if we
        // are in the middle of receiving a read burst and then issue a write,
        // the write might affect the read burst. This violates FIFO behaviour.
        // To solve this, we must wait until the last beat of a burst, but this
        // means that a TileLink master which performs early source reuse can
        // have one more transaction inflight than we promised AXI; stall it too.
        val maxCount = n.getOrElse(1)
        val count = RegInit(UInt(0, width = log2Ceil(maxCount + 1)))
        val write = Reg(Bool())
        val idle = count === UInt(0)

        val inc = as && out_arw.fire()
        val dec = ds && d_last && in.d.fire()
        count := count + inc.asUInt - dec.asUInt

        assert (!dec || count =/= UInt(0))        // underflow
        assert (!inc || count =/= UInt(maxCount)) // overflow

        //by dongdeji when (inc) { write := arw.wen }
        // If only one transaction can be inflight, it can't mismatch
        //by dongdeji val mismatch = if (maxCount > 1) { write =/= arw.wen } else { Bool(false) }
        //by dongdeji s := (!idle && mismatch) || (count === UInt(maxCount))
      }

      // Tie off unused channels
      in.b.valid := Bool(false)
      in.c.ready := Bool(true)
      in.e.ready := Bool(true)
    }
  }
}

object TLToSimple
{
  def apply(combinational: Boolean = true, adapterName: Option[String] = None, stripBits: Int = 0, wcorrupt: Boolean = true)(implicit p: Parameters) =
  {
    val tl2sramq = LazyModule(new TLToSimple(combinational, adapterName, stripBits, wcorrupt))
    tl2sramq.node
  }

  def sortByType(a: TLMasterParameters, b: TLMasterParameters): Boolean = {
    if ( a.supports.probe && !b.supports.probe) return false
    if (!a.supports.probe &&  b.supports.probe) return true
    if ( a.requestFifo    && !b.requestFifo   ) return false
    if (!a.requestFifo    &&  b.requestFifo   ) return true
    return false
  }
}
