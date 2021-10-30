// See LICENSE.SiFive for license details.

package freechips.rocketchip.amba.sramq

import Chisel._
import chisel3.DontCare
import chisel3.util.Irrevocable
import freechips.rocketchip.util._

abstract class SramQBundleBase(params: SramQBundleParameters) extends GenericParameterizedBundle(params)

class SramQWriteReqBundle(params: SramQBundleParameters) extends SramQBundleBase(params)
{
  val id = UInt(width = params.idBits)
  val addr = UInt(width = params.addrBits)
  val mask = UInt(width = params.dataBits/8)
  val data = UInt(width = params.dataBits) // added by dongdeji
}

class SramQWriteRspBundle(params: SramQBundleParameters) extends SramQBundleBase(params)
{
  val id = UInt(width = params.idBits)
  val addr = UInt(width = params.addrBits)
  val mask = UInt(width = params.dataBits/8)
  val data = UInt(width = params.dataBits)
}

class SramQReadReqBundle(params: SramQBundleParameters) extends SramQBundleBase(params)
{
  val id = UInt(width = params.idBits)
  val addr = UInt(width = params.addrBits)
  val mask = UInt(width = params.dataBits/8)
  val data = UInt(width = params.dataBits) // added by dongdeji
}

class SramQReadRspBundle(params: SramQBundleParameters) extends SramQBundleBase(params)
{
  val id = UInt(width = params.idBits)
  val addr = UInt(width = params.addrBits)
  val mask = UInt(width = params.dataBits/8)
  val data = UInt(width = params.dataBits)
}

class SramQBundle(params: SramQBundleParameters) extends SramQBundleBase(params)
{
  val wirte_req = Irrevocable(new SramQWriteReqBundle (params))
  val wirte_rsp = Irrevocable(new SramQWriteRspBundle (params)).flip
  val read_req = Irrevocable(new SramQReadReqBundle (params))
  val read_rsp = Irrevocable(new SramQReadRspBundle (params)).flip
}

object SramQBundle
{
  def apply(params: SramQBundleParameters) = new SramQBundle(params)
}

