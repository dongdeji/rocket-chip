// See LICENSE.SiFive for license details.

package freechips.rocketchip.amba.simple

import Chisel._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.diplomaticobjectmodel.logicaltree.{BusMemoryLogicalTreeNode, LogicalModuleTree, LogicalTreeNode}
import freechips.rocketchip.diplomaticobjectmodel.model.AXI4_Lite
import freechips.rocketchip.util._
import freechips.rocketchip.amba._

// Setting wcorrupt=true is not enough to enable the w.user field
// You must also list AMBACorrupt in your master's requestFields
class SimpleRAM(
    address: AddressSet,
    parentLogicalTreeNode: Option[LogicalTreeNode] = None,
    beatBytes: Int = 8,
    devName: Option[String] = None,
    wcorrupt: Boolean = true)
  (implicit p: Parameters) extends DiplomaticSRAM(address, beatBytes, devName)
{
  val node = SimpleSlaveNode(Seq(SimpleSlavePortParameters(
    Seq(SimpleSlaveParameters(
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
                                  size = BigInt(SimpleParameters.queue_depth),
                                  lanes = beatBytes,
                                  bits = laneDataBits)
    val eccCode = None
    val address = outer.address

    //val w_addr = Cat((mask zip (in.req.bits.addr >> log2Ceil(beatBytes)).asBools).filter(_._1).map(_._2).reverse)
    //val deq_sel = address.contains(in.deqreq.bits.addr)

    val head = RegInit(0.U(log2Up(SimpleParameters.queue_depth*2).W))
    val tail = RegInit(0.U(log2Up(SimpleParameters.queue_depth*2).W))
    /**** handle enq begin ****/
    def isFull = (tail(log2Up(SimpleParameters.queue_depth)-1,0) === (head + 1.U)(log2Up(SimpleParameters.queue_depth)-1,0))
    val enq_sel = address.contains(in.req.bits.addr)
    val rsped = RegInit(true.B)
    val req_s1 = RegInit(0.U.asTypeOf(chisel3.util.Valid(in.req.bits.cloneType)));chisel3.dontTouch(req_s1)
      
    in.rsp.valid := req_s1.valid
    in.rsp.bits.id := req_s1.bits.id
    in.rsp.bits.addr := req_s1.bits.addr
    in.rsp.bits.data := 100.U//mem.readAndHold(tail, true.B)
    when(in.rsp.fire()) {
      rsped := true.B
      req_s1.valid := false.B
    }

    when(!isFull && in.req.valid && enq_sel && rsped ) {
      rsped := false.B
      req_s1.valid := in.req.valid
      req_s1.bits := in.req.bits
    }

    in.req.ready := !isFull && rsped

    val wdata = Vec.tabulate(beatBytes) { i => in.req.bits.data(8*(i+1)-1, 8*i) }
    when (in.req.fire() && enq_sel && !isFull && 
            in.req.bits.opcode === SimpleParameters.OPCODE_ENQ) {
      mem.write(head, wdata, Fill(beatBytes, true.B).asBools)
      head := head + 1.U
    }
    /**** handle enq end ****/
  }
}

object SimpleRAM
{
  def apply(
    address: AddressSet,
    parentLogicalTreeNode: Option[LogicalTreeNode] = None,
    beatBytes: Int = 4,
    devName: Option[String] = None,
    wcorrupt: Boolean = true)
  (implicit p: Parameters) =
  {
    val sramqram = LazyModule(new SimpleRAM(
      address = address,
      beatBytes = beatBytes,
      devName = devName,
      wcorrupt = wcorrupt))
    sramqram.node
  }
}
