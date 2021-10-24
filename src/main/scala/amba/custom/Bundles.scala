// See LICENSE.SiFive for license details.
package freechips.rocketchip.amba.custom

import Chisel._
import chisel3.DontCare
import chisel3.util.Irrevocable
import freechips.rocketchip.util._


class CustomBundleA(params: CustomBundleParameters) extends Bundle
{
  val id     = UInt(width = params.idBits)
}

class CustomBundleB(params: CustomBundleParameters) extends Bundle
{
  val id   = UInt(width = params.idBits)
}

class CustomBundle(params: CustomBundleParameters) extends Bundle
{
  val a = Irrevocable(new CustomBundleA(params))
  val b = Irrevocable(new CustomBundleB(params)).flip
}

object CustomBundle
{
  def apply(params: CustomBundleParameters) = new CustomBundle(params)
}



