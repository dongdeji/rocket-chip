// See LICENSE.SiFive for license details.

package freechips.rocketchip.amba.custom

import Chisel._
import chisel3.DontCare
import chisel3.util.Irrevocable
import freechips.rocketchip.util._

abstract class CustomBundleBase(params: CustomBundleParameters) extends GenericParameterizedBundle(params)

class CustomEnqReqBundle(params: CustomBundleParameters) extends CustomBundleBase(params)
{
  val id = UInt(width = params.idBits)
  val addr = UInt(width = params.addrBits)
  val opcode = UInt(width = params.opcodeBits) // added by dongdeji  
  val data = UInt(width = params.dataBits) // added by dongdeji
}

class CustomEnqRspBundle(params: CustomBundleParameters) extends CustomBundleBase(params)
{
  val id = UInt(width = params.idBits)
  val addr = UInt(width = params.addrBits)
  val data = UInt(width = params.dataBits)
}

class CustomDeqReqBundle(params: CustomBundleParameters) extends CustomBundleBase(params)
{
  val id = UInt(width = params.idBits)
  val addr = UInt(width = params.addrBits)
  val opcode = UInt(width = params.opcodeBits) // added by dongdeji  
  val data = UInt(width = params.dataBits) // added by dongdeji
}

class CustomDeqRspBundle(params: CustomBundleParameters) extends CustomBundleBase(params)
{
  val id = UInt(width = params.idBits)
  val addr = UInt(width = params.addrBits)
  val data = UInt(width = params.dataBits)
}

class CustomBundle(params: CustomBundleParameters) extends CustomBundleBase(params)
{
  val enqreq = Irrevocable(new CustomEnqReqBundle (params))
  val enqrsp = Irrevocable(new CustomEnqRspBundle (params)).flip
  val deqreq = Irrevocable(new CustomDeqReqBundle (params))
  val deqrsp = Irrevocable(new CustomDeqRspBundle (params)).flip
}

object CustomBundle
{
  def apply(params: CustomBundleParameters) = new CustomBundle(params)
}

