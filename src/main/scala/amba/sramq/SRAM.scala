// See LICENSE.SiFive for license details.

package freechips.rocketchip.amba.sramq

import Chisel._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.diplomaticobjectmodel.logicaltree.{BusMemoryLogicalTreeNode, LogicalModuleTree, LogicalTreeNode}
import freechips.rocketchip.diplomaticobjectmodel.model.AXI4_Lite
import freechips.rocketchip.util._
import freechips.rocketchip.amba._

// Setting wcorrupt=true is not enough to enable the w.user field
// You must also list AMBACorrupt in your master's requestFields
class SramQRAM(
    address: AddressSet,
    parentLogicalTreeNode: Option[LogicalTreeNode] = None,
    beatBytes: Int = 8,
    devName: Option[String] = None,
    wcorrupt: Boolean = true)
  (implicit p: Parameters) extends DiplomaticSRAM(address, beatBytes, devName)
{
  val node = SramQSlaveNode(Seq(SramQSlavePortParameters(
    Seq(SramQSlaveParameters(
      address       = List(address),
      resources     = resources,
      supportsRead  = TransferSizes(1, beatBytes),
      supportsWrite = TransferSizes(1, beatBytes))),
    beatBytes  = beatBytes,
    minLatency = 1)))

  private val outer = this

  lazy val module = new LazyModuleImp(this) with HasJustOneSeqMem {
    val (in, edgeIn) = node.in(0)
    val laneDataBits = 8
    val (mem, omSRAM, omMem) = makeSinglePortedByteWriteSeqMem(
                                  size = BigInt(SramQParameters.queue_depth),
                                  lanes = beatBytes,
                                  bits = laneDataBits)
    val eccCode = None
    val address = outer.address

    //val w_addr = Cat((mask zip (in.wirte_req.bits.addr >> log2Ceil(beatBytes)).asBools).filter(_._1).map(_._2).reverse)
    //val read_sel = address.contains(in.read_req.bits.addr)

    val head = RegInit(0.U(log2Up(SramQParameters.queue_depth*2).W))
    val tail = RegInit(0.U(log2Up(SramQParameters.queue_depth*2).W))
    /**** handle enq begin ****/
    def isFull = (tail(log2Up(SramQParameters.queue_depth)-1,0) === (head + 1.U)(log2Up(SramQParameters.queue_depth)-1,0))
    val wirte_sel = address.contains(in.wirte_req.bits.addr)
    val wirte_rsped = RegInit(true.B)
    val wirte_reqs1 = RegInit(0.U.asTypeOf(chisel3.util.Valid(in.wirte_req.bits.cloneType)));chisel3.dontTouch(wirte_reqs1)
      
    in.wirte_rsp.valid := wirte_reqs1.valid
    in.wirte_rsp.bits.id := wirte_reqs1.bits.id
    in.wirte_rsp.bits.addr := wirte_reqs1.bits.addr
    in.wirte_rsp.bits.data := 100.U//mem.readAndHold(tail, true.B)
    when(in.wirte_rsp.fire()) {
      wirte_rsped := true.B
      wirte_reqs1.valid := false.B
    }

    when(!isFull && in.wirte_req.valid && wirte_sel && wirte_rsped ) {
      wirte_rsped := false.B
      wirte_reqs1.valid := in.wirte_req.valid
      wirte_reqs1.bits := in.wirte_req.bits
    }

    in.wirte_req.ready := !isFull && wirte_rsped

    val wdata = Vec.tabulate(beatBytes) { i => in.wirte_req.bits.data(8*(i+1)-1, 8*i) }
    when (in.wirte_req.fire() && wirte_sel && !isFull) {
      mem.write(head, wdata, Fill(beatBytes, true.B).asBools)
      head := head + 1.U
    }
    /**** handle enq end ****/

    /**** handle deq begin ****/
    def isEmpty = (tail(log2Up(SramQParameters.queue_depth)-1,0) === head(log2Up(SramQParameters.queue_depth)-1,0))
    val read_sel = address.contains(in.read_req.bits.addr)
    val read_rsped = RegInit(true.B)
    val read_reqs1 = RegInit(0.U.asTypeOf(chisel3.util.Valid(in.read_req.bits.cloneType)));chisel3.dontTouch(wirte_reqs1)

    in.read_rsp.valid := read_reqs1.valid
    in.read_rsp.bits.id := read_reqs1.bits.id
    in.read_rsp.bits.addr := read_reqs1.bits.addr
    in.read_rsp.bits.data := Cat(mem.readAndHold(tail, true.B).reverse)
    when(in.read_rsp.fire()) {
      read_rsped := true.B
      read_reqs1.valid := false.B
    }

    when(!isEmpty && in.read_req.fire() && read_sel && read_rsped ) {
      read_rsped := false.B
      read_reqs1.valid := true.B
      read_reqs1.bits := in.read_req.bits
      tail := tail + 1.U
    }

    in.read_req.ready := !isEmpty && read_rsped

    /**** handle deq end ****/
  }
}

object SramQRAM
{
  def apply(
    address: AddressSet,
    parentLogicalTreeNode: Option[LogicalTreeNode] = None,
    beatBytes: Int = 4,
    devName: Option[String] = None,
    wcorrupt: Boolean = true)
  (implicit p: Parameters) =
  {
    val sramqram = LazyModule(new SramQRAM(
      address = address,
      beatBytes = beatBytes,
      devName = devName,
      wcorrupt = wcorrupt))
    sramqram.node
  }
}
