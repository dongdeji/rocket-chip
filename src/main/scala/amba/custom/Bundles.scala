// See LICENSE.SiFive for license details.

package freechips.rocketchip.amba.custom

import Chisel._
import chisel3.DontCare
import chisel3.util.Irrevocable
import freechips.rocketchip.util._

abstract class CustomBundleBase(params: CustomBundleParameters) extends GenericParameterizedBundle(params)

class CustomBundleA(params: CustomBundleParameters) extends CustomBundleBase(params)
{
  val id     = UInt(width = params.idBits)
  val addr   = UInt(width = params.addrBits)
  val len    = UInt(width = params.lenBits)  // number of beats - 1
  val size   = UInt(width = params.sizeBits) // bytes in beat = 2^size
  val burst  = UInt(width = params.burstBits)
  val lock   = UInt(width = params.lockBits)
  val cache  = UInt(width = params.cacheBits)
  val prot   = UInt(width = params.protBits)
  val qos    = UInt(width = params.qosBits)  // 0=no QoS, bigger = higher priority
}

class CustomBundleB(params: CustomBundleParameters) extends CustomBundleBase(params)
{
  val id   = UInt(width = params.idBits)
  val resp = UInt(width = params.respBits)
  val user = BundleMap(params.responseFields.filter(_.key.isControl))
  val echo = BundleMap(params.echoFields)
}

class CustomBundle(params: CustomBundleParameters) extends CustomBundleBase(params)
{
  val a  = Irrevocable(new CustomBundleA (params))
  val b  = Irrevocable(new CustomBundleB (params)).flip
}

object CustomBundle
{
  def apply(params: CustomBundleParameters) = new CustomBundle(params)
}

