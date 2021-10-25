// See LICENSE.SiFive for license details.

package freechips.rocketchip.amba.custom

import Chisel._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.diplomaticobjectmodel.logicaltree.{BusMemoryLogicalTreeNode, LogicalModuleTree, LogicalTreeNode}
import freechips.rocketchip.diplomaticobjectmodel.model.AXI4_Lite
import freechips.rocketchip.util._
import freechips.rocketchip.amba._

// Setting wcorrupt=true is not enough to enable the w.user field
// You must also list AMBACorrupt in your master's requestFields
class CustomRAM(
    address: AddressSet,
    //by dongdeji cacheable: Boolean = true,
    parentLogicalTreeNode: Option[LogicalTreeNode] = None,
    //by dongdeji executable: Boolean = true,
    beatBytes: Int = 4,
    devName: Option[String] = None,
    errors: Seq[AddressSet] = Nil,
    wcorrupt: Boolean = true)
  (implicit p: Parameters) extends DiplomaticSRAM(address, beatBytes, devName)
{
  val node = CustomSlaveNode(Seq(CustomSlavePortParameters(
    Seq(CustomSlaveParameters(
      address       = List(address) ++ errors,
      resources     = resources,
      //by dongdeji regionType    = if (cacheable) RegionType.UNCACHED else RegionType.IDEMPOTENT,
      //by dongdeji executable    = executable,
      supportsRead  = TransferSizes(1, beatBytes),
      supportsWrite = TransferSizes(1, beatBytes)/*,
      interleavedId = Some(0)*/)),
    beatBytes  = beatBytes,
    //by dongdeji requestKeys = if (wcorrupt) Seq(AMBACorrupt) else Seq(),
    minLatency = 1)))

  private val outer = this

  lazy val module = new LazyModuleImp(this) with HasJustOneSeqMem {
    val (in, edgeIn) = node.in(0)
    val laneDataBits = 8
    val (mem, omSRAM, omMem) = makeSinglePortedByteWriteSeqMem(
      size = BigInt(1) << mask.filter(b=>b).size,
      lanes = beatBytes,
      bits = laneDataBits)
    val eccCode = None
    val address = outer.address

    //by dongdeji parentLogicalTreeNode.map {
    //by dongdeji   case parentLTN =>
    //by dongdeji     def sramLogicalTreeNode = new BusMemoryLogicalTreeNode(
    //by dongdeji       device = device,
    //by dongdeji       omSRAMs = Seq(omSRAM),
    //by dongdeji       busProtocol = new AXI4_Lite(None),
    //by dongdeji       dataECC = None,
    //by dongdeji       hasAtomics = None,
    //by dongdeji       busProtocolSpecification = None)
    //by dongdeji     LogicalModuleTree.add(parentLTN, sramLogicalTreeNode)
    //by dongdeji }

    //by dongdeji val corrupt = if (edgeIn.bundle.requestFields.contains(AMBACorrupt)) Some(SeqMem(1 << mask.filter(b=>b).size, UInt(width=2))) else None

    //by dongdeji val r_addr = Cat((mask zip (in.ar.bits.addr >> log2Ceil(beatBytes)).asBools).filter(_._1).map(_._2).reverse)
    val w_addr = Cat((mask zip (in.a.bits.addr >> log2Ceil(beatBytes)).asBools).filter(_._1).map(_._2).reverse)
    //by dongdeji val r_sel0 = address.contains(in.ar.bits.addr)
    val w_sel0 = address.contains(in.a.bits.addr)

    val w_full = RegInit(Bool(false))
    val w_id   = Reg(UInt())
    //by dongdeji val w_echo = Reg(BundleMap(in.params.echoFields))
    //by dongdeji val r_sel1 = Reg(r_sel0)
    val w_sel1 = Reg(w_sel0)

    when (in.b.fire()) { w_full := Bool(false) }
    when (in.a.fire()) { w_full := Bool(true) }

    when (in.a.fire()) {
      w_id := in.a.bits.id
      w_sel1 := w_sel0
      //by dongdeji w_echo :<= in.a.bits.echo
    }

    //val wdata = Vec.tabulate(beatBytes) { i => in.w.bits.data(8*(i+1)-1, 8*i) }
    when (in.a.fire() && w_sel0) {
      //by dongdeji mem.write(w_addr, wdata, in.w.bits.strb.asBools)
      //by dongdeji corrupt.foreach { _.write(w_addr, in.w.bits.user(AMBACorrupt).asUInt) }
    }

    //by dongdeji in. b.valid := w_full
    //by dongdeji in.a.ready := in. w.valid && (in.b.ready || !w_full)
    //by dongdeji in. w.ready := in.aw.valid && (in.b.ready || !w_full)

    in.b.bits.id   := w_id
    in.b.bits.resp := 10.U//by dongdeji Mux(w_sel1, CustomParameters.RESP_OKAY, CustomParameters.RESP_DECERR)
    //by dongdeji in.b.bits.echo :<= w_echo

    //by dongdeji val r_full = RegInit(Bool(false))
    //by dongdeji val r_id   = Reg(UInt())
    //by dongdeji val r_echo = Reg(BundleMap(in.params.echoFields))

    //by dongdeji when (in. r.fire()) { r_full := Bool(false) }
    //by dongdeji when (in.ar.fire()) { r_full := Bool(true) }

    //by dongdeji when (in.ar.fire()) {
    //by dongdeji   r_id := in.ar.bits.id
    //by dongdeji   r_sel1 := r_sel0
    //by dongdeji   r_echo :<= in.ar.bits.echo
    //by dongdeji }

    //by dongdeji val ren = in.ar.fire()
    //by dongdeji val rdata = mem.readAndHold(r_addr, ren)
    //by dongdeji val rcorrupt = corrupt.map(_.readAndHold(r_addr, ren)(0)).getOrElse(Bool(false))

    //by dongdeji in. r.valid := r_full
    //by dongdeji in.ar.ready := in.r.ready || !r_full

    //by dongdeji in.r.bits.id   := r_id
    //by dongdeji in.r.bits.resp := Mux(r_sel1, Mux(rcorrupt, CustomParameters.RESP_SLVERR, CustomParameters.RESP_OKAY), CustomParameters.RESP_DECERR)
    //by dongdeji in.r.bits.data := Cat(rdata.reverse)
    //by dongdeji in.r.bits.echo :<= r_echo
    //by dongdeji in.r.bits.last := Bool(true)
  }
}

object CustomRAM
{
  def apply(
    address: AddressSet,
    parentLogicalTreeNode: Option[LogicalTreeNode] = None,
    beatBytes: Int = 4,
    devName: Option[String] = None,
    errors: Seq[AddressSet] = Nil,
    wcorrupt: Boolean = true)
  (implicit p: Parameters) =
  {
    val customram = LazyModule(new CustomRAM(
      address = address,
      beatBytes = beatBytes,
      devName = devName,
      errors = errors,
      wcorrupt = wcorrupt))
    customram.node
  }
}
