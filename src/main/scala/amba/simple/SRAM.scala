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
    enqaddress: AddressSet,
    deqaddress: AddressSet,
    parentLogicalTreeNode: Option[LogicalTreeNode] = None,
    beatBytes: Int = 8,
    devName: Option[String] = None,
    wcorrupt: Boolean = true)
  (implicit p: Parameters) extends DiplomaticSRAM(enqaddress, beatBytes, devName)
{
  val enqnode = SimpleSlaveNode(Seq(SimpleSlavePortParameters(
    Seq(SimpleSlaveParameters(
      address       = List(enqaddress),
      resources     = resources,
      supportsRead  = TransferSizes(1, beatBytes),
      supportsWrite = TransferSizes(1, beatBytes))),
    beatBytes  = beatBytes,
    minLatency = 1)))

  val deqnode = SimpleSlaveNode(Seq(SimpleSlavePortParameters(
    Seq(SimpleSlaveParameters(
      address       = List(deqaddress),
      resources     = resources,
      supportsRead  = TransferSizes(1, beatBytes),
      supportsWrite = TransferSizes(1, beatBytes))),
    beatBytes  = beatBytes,
    minLatency = 1)))

  private val outer = this

  lazy val module = new LazyModuleImp(this) with HasJustOneSeqMem {
    val (enqin, enqedgeIn) = enqnode.in(0)
    val (deqin, deqedgeIn) = deqnode.in(0)
    val laneDataBits = 8
    val (mem, omSRAM, omMem) = makeSinglePortedByteWriteSeqMem(
                                  size = BigInt(SimpleParameters.queue_depth),
                                  lanes = beatBytes,
                                  bits = laneDataBits)
    val eccCode = None
    val address = outer.address

    //val w_addr = Cat((mask zip (enqin.req.bits.addr >> log2Ceil(beatBytes)).asBools).filter(_._1).map(_._2).reverse)
    //val deq_sel = address.contains(enqin.deqreq.bits.addr)

    val head = RegInit(0.U(log2Up(SimpleParameters.queue_depth*2).W))
    val tail = RegInit(0.U(log2Up(SimpleParameters.queue_depth*2).W))

    /**** handle enq begin ****/
    def isFull = (tail(log2Up(SimpleParameters.queue_depth)-1,0) === (head + 1.U)(log2Up(SimpleParameters.queue_depth)-1,0))
    val enq_sel = address.contains(enqin.req.bits.addr)
    val enq_rsped = RegInit(true.B)
    val enq_req_s1 = RegInit(0.U.asTypeOf(chisel3.util.Valid(enqin.req.bits.cloneType)));chisel3.dontTouch(enq_req_s1)
      
    enqin.rsp.valid := enq_req_s1.valid
    enqin.rsp.bits.id := enq_req_s1.bits.id
    enqin.rsp.bits.addr := enq_req_s1.bits.addr
    enqin.rsp.bits.data := 100.U//mem.readAndHold(tail, true.B)
    when(enqin.rsp.fire()) {
      enq_rsped := true.B
      enq_req_s1.valid := false.B
    }

    when(!isFull && enqin.req.valid && enq_sel && enq_rsped ) {
      enq_rsped := false.B
      enq_req_s1.valid := enqin.req.valid
      enq_req_s1.bits := enqin.req.bits
    }

    enqin.req.ready := !isFull && enq_rsped

    val wdata = Vec.tabulate(beatBytes) { i => enqin.req.bits.data(8*(i+1)-1, 8*i) }
    when (enqin.req.fire() && enq_sel && !isFull && 
            enqin.req.bits.opcode === SimpleParameters.OPCODE_ENQ) {
      mem.write(head, wdata, Fill(beatBytes, true.B).asBools)
      head := head + 1.U
    }
    /**** handle enq end ****/

    /**** handle deq begin ****/
    def isEmpty = (tail(log2Up(SimpleParameters.queue_depth)-1,0) === head(log2Up(SimpleParameters.queue_depth)-1,0))
    val deq_sel = address.contains(deqin.req.bits.addr)
    val deq_rsped = RegInit(true.B)
    val deq_req_s1 = RegInit(0.U.asTypeOf(chisel3.util.Valid(deqin.req.bits.cloneType)));chisel3.dontTouch(deq_req_s1)
      
    deqin.rsp.valid := deq_req_s1.valid
    deqin.rsp.bits.id := deq_req_s1.bits.id
    deqin.rsp.bits.addr := deq_req_s1.bits.addr
    deqin.rsp.bits.data := Cat(mem.readAndHold(tail, true.B).reverse)
    when(deqin.rsp.fire()) {
      deq_rsped := true.B
      deq_req_s1.valid := false.B
    }

    when(!isEmpty && deqin.req.valid && deq_sel && deq_rsped ) {
      deq_rsped := false.B
      deq_req_s1.valid := deqin.req.valid
      deq_req_s1.bits := deqin.req.bits
      tail := tail + 1.U
    }

    deqin.req.ready := !isEmpty && deq_rsped

    /**** handle deq end ****/


  }
}

object SimpleRAM
{
  def apply(
    enqaddress: AddressSet,
    deqaddress: AddressSet,
    parentLogicalTreeNode: Option[LogicalTreeNode] = None,
    beatBytes: Int = 4,
    devName: Option[String] = None,
    wcorrupt: Boolean = true)
  (implicit p: Parameters) =
  {
    val sramqram = LazyModule(new SimpleRAM(
      enqaddress = enqaddress,
      deqaddress = deqaddress,
      beatBytes = beatBytes,
      devName = devName,
      wcorrupt = wcorrupt))
    (sramqram.enqnode, sramqram.deqnode)
  }
}
