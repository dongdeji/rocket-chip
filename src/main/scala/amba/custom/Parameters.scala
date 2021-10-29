// See LICENSE.SiFive for license details.

package freechips.rocketchip.amba.custom

import Chisel._
import chisel3.internal.sourceinfo.SourceInfo
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util._
import scala.math.max

case class CustomSlaveParameters(
  address:       Seq[AddressSet],
  resources:     Seq[Resource] = Nil,
  nodePath:      Seq[BaseNode] = Seq(),
  supportsWrite: TransferSizes = TransferSizes.none,
  supportsRead:  TransferSizes = TransferSizes.none,
  device: Option[Device] = None) // The device will not interleave responses (R+B)
{
  address.foreach { a => require (a.finite) }
  address.combinations(2).foreach { case Seq(x,y) => require (!x.overlaps(y), s"$x and $y overlap") }

  val name = nodePath.lastOption.map(_.lazyModule.name).getOrElse("disconnected")
  val maxTransfer = max(supportsWrite.max, supportsRead.max)
  val maxAddress = address.map(_.max).max
  val minAlignment = address.map(_.alignment).min

  // The device had better not support a transfer larger than its alignment
  require (minAlignment >= maxTransfer,
    s"minAlignment ($minAlignment) must be >= maxTransfer ($maxTransfer)")

}

case class CustomSlavePortParameters(
  slaves:     Seq[CustomSlaveParameters],
  beatBytes:  Int,
  minLatency: Int = 1)
{
  require (!slaves.isEmpty)
  require (isPow2(beatBytes))

  val maxTransfer = slaves.map(_.maxTransfer).max
  val maxAddress = slaves.map(_.maxAddress).max

  // Check the link is not pointlessly wide
  require (maxTransfer >= beatBytes,
    s"maxTransfer ($maxTransfer) should not be smaller than bus width ($beatBytes)")
  // Check that the link can be implemented in Custom

  // Require disjoint ranges for addresses
  slaves.combinations(2).foreach { case Seq(x,y) =>
    x.address.foreach { a => y.address.foreach { b =>
      require (!a.overlaps(b), s"$a and $b overlap")
    } }
  }
}

case class CustomMasterParameters(
  name:      String,
  id:        IdRange       = IdRange(0, 1),
  maxFlight: Option[Int]   = None, // None = infinite, else is a per-ID cap
  nodePath:  Seq[BaseNode] = Seq())
{
  maxFlight.foreach { m => require (m >= 0) }
}

case class CustomMasterPortParameters(
  masters:    Seq[CustomMasterParameters])
{
  val endId = masters.map(_.id.end).max

  // Require disjoint ranges for ids
  IdRange.overlaps(masters.map(_.id)).foreach { case (x, y) =>
    require (!x.overlaps(y), s"CustomMasterParameters.id $x and $y overlap")
  }
}

case class CustomBundleParameters(
  addrBits: Int,
  dataBits: Int,
  idBits:   Int)
{
  require (dataBits >= 8, s"Custom data bits must be >= 8 (got $dataBits)")
  require (addrBits >= 1, s"Custom addr bits must be >= 1 (got $addrBits)")
  require (idBits >= 1, s"Custom id bits must be >= 1 (got $idBits)")
  require (isPow2(dataBits), s"Custom data bits must be pow2 (got $dataBits)")

  // Bring the globals into scope
  val respBits = CustomParameters.respBits
  val opcodeBits = CustomParameters.opcodeBits

  def union(x: CustomBundleParameters) =
    CustomBundleParameters(
      max(addrBits,   x.addrBits),
      max(dataBits,   x.dataBits),
      max(idBits,     x.idBits))
}

object CustomBundleParameters
{
  val emptyBundleParams = CustomBundleParameters(addrBits=1, dataBits=8, idBits=1/*, echoFields=Nil, requestFields=Nil, responseFields=Nil*/)
  def union(x: Seq[CustomBundleParameters]) = x.foldLeft(emptyBundleParams)((x,y) => x.union(y))

  def apply(master: CustomMasterPortParameters, slave: CustomSlavePortParameters) =
    new CustomBundleParameters(
      addrBits = log2Up(slave.maxAddress+1),
      dataBits = slave.beatBytes * 8,
      idBits   = log2Up(master.endId))
}

case class CustomEdgeParameters(
  master: CustomMasterPortParameters,
  slave:  CustomSlavePortParameters,
  params: Parameters,
  sourceInfo: SourceInfo)
{
  val bundle = CustomBundleParameters(master, slave)
}

case class CustomBufferParams(
  a: BufferParams = BufferParams.none,
  b: BufferParams = BufferParams.none
) extends DirectedBuffers[CustomBufferParams] {
  def copyIn(x: BufferParams) = this.copy(b = x )
  def copyOut(x: BufferParams) = this.copy(a = x )
  def copyInOut(x: BufferParams) = this.copyIn(x).copyOut(x)
}

/** Pretty printing of Custom source id maps */
class CustomIdMap(axi4: CustomMasterPortParameters) extends IdMap[CustomIdMapEntry] {
  private val axi4Digits = String.valueOf(axi4.endId-1).length()
  protected val fmt = s"\t[%${axi4Digits}d, %${axi4Digits}d) %s%s%s"
  private val sorted = axi4.masters.sortBy(_.id)

  val mapping: Seq[CustomIdMapEntry] = sorted.map { case c =>
    // to conservatively state max number of transactions, assume every id has up to c.maxFlight and reuses ids between AW and AR channels
    val maxTransactionsInFlight = c.maxFlight.map(_ * c.id.size * 2)
    CustomIdMapEntry(c.id, c.name, maxTransactionsInFlight)
  }
}

case class CustomIdMapEntry(axi4Id: IdRange, name: String, maxTransactionsInFlight: Option[Int] = None) extends IdMapEntry {
  val from = axi4Id
  val to = axi4Id
  val isCache = false
  val requestFifo = false
}
