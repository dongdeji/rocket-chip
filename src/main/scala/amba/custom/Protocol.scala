// See LICENSE.SiFive for license details.

package freechips.rocketchip.amba.custom

import Chisel._

object CustomParameters
{
  // These are all fixed by the Custom standard:
  val lenBits   = 8
  val sizeBits  = 3
  val burstBits = 2
  val lockBits  = 1
  val cacheBits = 4
  val protBits  = 3
  val qosBits   = 4
  val respBits  = 2
}
