// See LICENSE.SiFive for license details.

package freechips.rocketchip.amba.simple

import Chisel._
import chisel3.DontCare
import chisel3.util.Irrevocable
import freechips.rocketchip.util._

abstract class SimpleBundleBase(params: SimpleBundleParameters) extends GenericParameterizedBundle(params)

class SimpleReqBundle(params: SimpleBundleParameters) extends SimpleBundleBase(params)
{
  val id = UInt(width = params.idBits)
  val addr = UInt(width = params.addrBits)
  val opcode = UInt(width = params.opcodeBits) // added by dongdeji  
  val data = UInt(width = params.dataBits) // added by dongdeji
}

class SimpleRspBundle(params: SimpleBundleParameters) extends SimpleBundleBase(params)
{
  val id = UInt(width = params.idBits)
  val addr = UInt(width = params.addrBits)
  val data = UInt(width = params.dataBits)
}

class SimpleDeqReqBundle(params: SimpleBundleParameters) extends SimpleBundleBase(params)
{
  val id = UInt(width = params.idBits)
  val addr = UInt(width = params.addrBits)
  val opcode = UInt(width = params.opcodeBits) // added by dongdeji  
  val data = UInt(width = params.dataBits) // added by dongdeji
}

class SimpleDeqRspBundle(params: SimpleBundleParameters) extends SimpleBundleBase(params)
{
  val id = UInt(width = params.idBits)
  val addr = UInt(width = params.addrBits)
  val data = UInt(width = params.dataBits)
}

class SimpleBundle(params: SimpleBundleParameters) extends SimpleBundleBase(params)
{
  val req = Irrevocable(new SimpleReqBundle (params))
  val rsp = Irrevocable(new SimpleRspBundle (params)).flip
  val deqreq = Irrevocable(new SimpleDeqReqBundle (params))
  val deqrsp = Irrevocable(new SimpleDeqRspBundle (params)).flip
}

object SimpleBundle
{
  def apply(params: SimpleBundleParameters) = new SimpleBundle(params)
}

