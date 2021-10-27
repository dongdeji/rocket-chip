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
    parentLogicalTreeNode: Option[LogicalTreeNode] = None,
    beatBytes: Int = 8,
    devName: Option[String] = None,
    errors: Seq[AddressSet] = Nil,
    wcorrupt: Boolean = true)
  (implicit p: Parameters) extends DiplomaticSRAM(address, beatBytes, devName)
{
  val node = CustomSlaveNode(Seq(CustomSlavePortParameters(
    Seq(CustomSlaveParameters(
      address       = List(address) ++ errors,
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
                                  size = BigInt(CustomParameters.queue_depth),
                                  lanes = beatBytes,
                                  bits = laneDataBits)
    val eccCode = None
    val address = outer.address

    //val w_addr = Cat((mask zip (in.enqreq.bits.addr >> log2Ceil(beatBytes)).asBools).filter(_._1).map(_._2).reverse)
    //val deq_sel = address.contains(in.deqreq.bits.addr)

    val head = RegInit(0.U(log2Up(CustomParameters.queue_depth*2).W))
    val tail = RegInit(0.U(log2Up(CustomParameters.queue_depth*2).W))
    /**** handle enq begin ****/
    def isFull = (tail(log2Up(CustomParameters.queue_depth)-1,0) === (head + 1.U)(log2Up(CustomParameters.queue_depth)-1,0))
    val enq_sel = address.contains(in.enqreq.bits.addr)
    val enqrsped = RegInit(true.B)
    val enqreq_s1 = RegInit(0.U.asTypeOf(chisel3.util.Valid(in.enqreq.bits.cloneType)));chisel3.dontTouch(enqreq_s1)
      
    in.enqrsp.valid := enqreq_s1.valid
    in.enqrsp.bits.id := enqreq_s1.bits.id
    in.enqrsp.bits.addr := enqreq_s1.bits.addr
    in.enqrsp.bits.data := 100.U//mem.readAndHold(tail, true.B)
    when(in.enqrsp.fire()) {
      enqrsped := true.B
      enqreq_s1.valid := false.B
    }

    when(!isFull && in.enqreq.valid && enq_sel && enqrsped ) {
      enqrsped := false.B
      enqreq_s1.valid := in.enqreq.valid
      enqreq_s1.bits := in.enqreq.bits
    }

    in.enqreq.ready := !isFull && enqrsped

    val wdata = Vec.tabulate(beatBytes) { i => in.enqreq.bits.data(8*(i+1)-1, 8*i) }
    when (in.enqreq.fire() && enq_sel && !isFull && 
            in.enqreq.bits.opcode === CustomParameters.OPCODE_ENQ) {
      mem.write(head, wdata, Fill(beatBytes, true.B).asBools)
      head := head + 1.U
    }
    /**** handle enq end ****/

    /**** handle deq begin ****/
    def isEmpty = (tail(log2Up(CustomParameters.queue_depth)-1,0) === head(log2Up(CustomParameters.queue_depth)-1,0))
    val deq_sel = address.contains(in.deqreq.bits.addr)
    val deqrsped = RegInit(true.B)
    val deqreq_s1 = RegInit(0.U.asTypeOf(chisel3.util.Valid(in.deqreq.bits.cloneType)));chisel3.dontTouch(enqreq_s1)
    
    in.deqrsp.valid := enqreq_s1.valid
    in.deqrsp.bits.id := enqreq_s1.bits.id
    in.deqrsp.bits.addr := enqreq_s1.bits.addr
    in.deqrsp.bits.data := Cat(mem.readAndHold(tail, true.B).reverse)
    when(in.deqrsp.fire()) {
      enqrsped := true.B
      enqreq_s1.valid := false.B
    }

    when(!isFull && in.deqreq.valid && deq_sel && deqrsped ) {
      deqrsped := false.B
      deqreq_s1.valid := in.deqreq.valid
      deqreq_s1.bits := in.deqreq.bits
    }

    in.deqreq.ready := !isEmpty && deqrsped

    when (in.deqreq.fire() && deq_sel && !isEmpty && 
            in.deqreq.bits.opcode === CustomParameters.OPCODE_DEQ) {
      tail := tail + 1.U
    }
    /**** handle deq end ****/
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
