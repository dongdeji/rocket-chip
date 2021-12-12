// See LICENSE.SiFive for license details.

package freechips.rocketchip.amba.axi4

import Chisel._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.diplomaticobjectmodel.logicaltree.{BusMemoryLogicalTreeNode, LogicalModuleTree, LogicalTreeNode}
import freechips.rocketchip.diplomaticobjectmodel.model.AXI4_Lite
import freechips.rocketchip.util._
import freechips.rocketchip.amba._
import java.nio.file.{Files, Paths}
import java.nio.ByteBuffer

class FileData(contentsDelayed: => Seq[Byte] ) { def Bytes = contentsDelayed }

// Setting wcorrupt=true is not enough to enable the w.user field
// You must also list AMBACorrupt in your master's requestFields
class AXI4ROM(
    address: AddressSet,
    cacheable: Boolean = true,
    parentLogicalTreeNode: Option[LogicalTreeNode] = None,
    executable: Boolean = true,
    beatBytes: Int = 4,
    devName: Option[String] = None,
    errors: Seq[AddressSet] = Nil,
    wcorrupt: Boolean = true)
  (implicit p: Parameters) extends LazyModule
{
  val device = devName
    .map(new SimpleDevice(_, Seq("sifive,sram0")))
    .getOrElse(new MemoryDevice())

  val resources = device.reg("mem")

  val node = AXI4SlaveNode(Seq(AXI4SlavePortParameters(
    Seq(AXI4SlaveParameters(
      address       = List(address) ++ errors,
      resources     = resources,
      regionType    = if (cacheable) RegionType.UNCACHED else RegionType.IDEMPOTENT,
      executable    = executable,
      supportsRead  = TransferSizes(1, beatBytes),
      supportsWrite = TransferSizes(1, beatBytes),
      interleavedId = Some(0))),
    beatBytes  = beatBytes,
    requestKeys = if (wcorrupt) Seq(AMBACorrupt) else Seq(),
    minLatency = 1)))

  lazy val module = new LazyModuleImp(this) {
    val (in, edgeIn) = node.in(0)

    private lazy val img_contents = {
              val romdata = Files.readAllBytes(Paths.get("./bootrom/bootrom.img"))
              val rom = ByteBuffer.wrap(romdata)
              rom.array()}

    val contents = new FileData(img_contents).Bytes
    val wrapSize = 1 << log2Ceil(contents.size)
    //require (wrapSize <= depth*datalen)
    val dwords = (contents ++ Seq.fill(wrapSize-contents.size)(0.toByte)).grouped(8).toSeq
    val bigs = dwords.map(_.foldRight(BigInt(0)){ case (x,y) => (x.toInt & 0xff) | y << 8})
    val rom = Chisel.Vec(bigs.map(x => x.U((8*8).W)))
    
    val ar_sel_s0 = address.contains(in.ar.bits.addr)
    val ar_sel_s1 = RegNext(ar_sel_s0)
    val ar_fire_s1 = RegNext(in.ar.fire())
    val ar_id_s1 = RegNext(in.ar.bits.id)
    val ar_echo_s1 = RegNext(in.ar.bits.echo)
    val r_data = RegNext(rom(in.ar.bits.addr >> 3))
    
    in.ar.ready := true.B
    in.r.valid  := ar_fire_s1
    in.r.bits.id   := ar_id_s1
    in.r.bits.resp := Mux(ar_sel_s1, Mux(/*rcorrupt*/false.B, AXI4Parameters.RESP_SLVERR, AXI4Parameters.RESP_OKAY), AXI4Parameters.RESP_DECERR)
    in.r.bits.data := r_data
    in.r.bits.echo := ar_echo_s1
    in.r.bits.last := Bool(true)

  }
}

object AXI4ROM
{
  def apply(
    address: AddressSet,
    cacheable: Boolean = true,
    parentLogicalTreeNode: Option[LogicalTreeNode] = None,
    executable: Boolean = true,
    beatBytes: Int = 4,
    devName: Option[String] = None,
    errors: Seq[AddressSet] = Nil,
    wcorrupt: Boolean = true)
  (implicit p: Parameters) =
  {
    val axi4rom = LazyModule(new AXI4RAM(
      address = address,
      cacheable = cacheable,
      executable = executable,
      beatBytes = beatBytes,
      devName = devName,
      errors = errors,
      wcorrupt = wcorrupt))
    axi4rom.node
  }
}
