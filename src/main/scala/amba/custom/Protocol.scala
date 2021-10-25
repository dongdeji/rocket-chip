// See LICENSE.SiFive for license details.

package freechips.rocketchip.amba.custom

import Chisel._

object CustomParameters
{
  // These are all fixed by the Custom standard:
  //by dongdeji val lenBits   = 8
  //by dongdeji val sizeBits  = 3
  //by dongdeji val burstBits = 2
  //by dongdeji val lockBits  = 1
  //by dongdeji val cacheBits = 4
  //by dongdeji val protBits  = 3
  //by dongdeji val qosBits   = 4
  val respBits  = 32

  //by dongdeji def CACHE_RALLOCATE  = UInt(8, width = cacheBits)
  //by dongdeji def CACHE_WALLOCATE  = UInt(4, width = cacheBits)
  //by dongdeji def CACHE_MODIFIABLE = UInt(2, width = cacheBits)
  //by dongdeji def CACHE_BUFFERABLE = UInt(1, width = cacheBits)

  //by dongdeji def PROT_PRIVILEDGED = UInt(1, width = protBits)
  //by dongdeji def PROT_INSECURE    = UInt(2, width = protBits)
  //by dongdeji def PROT_INSTRUCTION = UInt(4, width = protBits)

  //by dongdeji def BURST_FIXED = UInt(0, width = burstBits)
  //by dongdeji def BURST_INCR  = UInt(1, width = burstBits)
  //by dongdeji def BURST_WRAP  = UInt(2, width = burstBits)

  //by dongdeji def RESP_OKAY   = UInt(0, width = respBits)
  //by dongdeji def RESP_EXOKAY = UInt(1, width = respBits)
  //by dongdeji def RESP_SLVERR = UInt(2, width = respBits)
  //by dongdeji def RESP_DECERR = UInt(3, width = respBits)
}
