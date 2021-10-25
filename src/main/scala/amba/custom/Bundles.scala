// See LICENSE.SiFive for license details.

package freechips.rocketchip.amba.custom

import Chisel._
import chisel3.DontCare
import chisel3.util.Irrevocable
import freechips.rocketchip.util._

abstract class CustomBundleBase(params: CustomBundleParameters) extends GenericParameterizedBundle(params)

class CustomBundleA(params: CustomBundleParameters) extends CustomBundleBase(params)
{
  val id = UInt(width = params.idBits)
  val addr = UInt(width = params.addrBits)
  val opcode = UInt(width = 32) // added by dongdeji
}

class CustomBundleB(params: CustomBundleParameters) extends CustomBundleBase(params)
{
  val id = UInt(width = params.idBits)
  val addr = UInt(width = params.addrBits)
  val resp = UInt(width = params.respBits)
}

class CustomBundle(params: CustomBundleParameters) extends CustomBundleBase(params)
{
  val a = Irrevocable(new CustomBundleA (params))
  val b = Irrevocable(new CustomBundleB (params)).flip
}

object CustomBundle
{
  def apply(params: CustomBundleParameters) = new CustomBundle(params)
}

