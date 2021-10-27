// See LICENSE.SiFive for license details.

package freechips.rocketchip.amba.sramq

import Chisel._
import chisel3.DontCare
import chisel3.util.Irrevocable
import freechips.rocketchip.util._

abstract class SramQBundleBase(params: SramQBundleParameters) extends GenericParameterizedBundle(params)

class SramQEnqReqBundle(params: SramQBundleParameters) extends SramQBundleBase(params)
{
  val id = UInt(width = params.idBits)
  val addr = UInt(width = params.addrBits)
  val opcode = UInt(width = params.opcodeBits) // added by dongdeji  
  val data = UInt(width = params.dataBits) // added by dongdeji
}

class SramQEnqRspBundle(params: SramQBundleParameters) extends SramQBundleBase(params)
{
  val id = UInt(width = params.idBits)
  val addr = UInt(width = params.addrBits)
  val data = UInt(width = params.dataBits)
}

class SramQDeqReqBundle(params: SramQBundleParameters) extends SramQBundleBase(params)
{
  val id = UInt(width = params.idBits)
  val addr = UInt(width = params.addrBits)
  val opcode = UInt(width = params.opcodeBits) // added by dongdeji  
  val data = UInt(width = params.dataBits) // added by dongdeji
}

class SramQDeqRspBundle(params: SramQBundleParameters) extends SramQBundleBase(params)
{
  val id = UInt(width = params.idBits)
  val addr = UInt(width = params.addrBits)
  val data = UInt(width = params.dataBits)
}

class SramQBundle(params: SramQBundleParameters) extends SramQBundleBase(params)
{
  val enqreq = Irrevocable(new SramQEnqReqBundle (params))
  val enqrsp = Irrevocable(new SramQEnqRspBundle (params)).flip
  val deqreq = Irrevocable(new SramQDeqReqBundle (params))
  val deqrsp = Irrevocable(new SramQDeqRspBundle (params)).flip
}

object SramQBundle
{
  def apply(params: SramQBundleParameters) = new SramQBundle(params)
}

