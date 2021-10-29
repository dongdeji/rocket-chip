// See LICENSE.SiFive for license details.

package freechips.rocketchip.amba.simple

import Chisel._

object SimpleParameters
{
  val respBits = 64
  val opcodeBits = 32
  val queue_depth = 1024
  require (isPow2(queue_depth))

  def OPCODE_ENQ = UInt(1, width = opcodeBits)
  def OPCODE_DEQ = UInt(2, width = opcodeBits)
}
