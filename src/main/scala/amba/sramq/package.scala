// See LICENSE.SiFive for license details.

package freechips.rocketchip.amba

import freechips.rocketchip.diplomacy.{HasClockDomainCrossing, _}
import freechips.rocketchip.prci.{HasResetDomainCrossing}

package object sramq
{
  type SramQNode = SimpleNodeHandle[SramQMasterPortParameters, SramQSlavePortParameters, SramQEdgeParameters, SramQBundle]
  type SramQOutwardNode = OutwardNodeHandle[SramQMasterPortParameters, SramQSlavePortParameters, SramQEdgeParameters, SramQBundle]
  type SramQInwardNode = InwardNodeHandle[SramQMasterPortParameters, SramQSlavePortParameters, SramQEdgeParameters, SramQBundle]

  implicit class SramQClockDomainCrossing(private val x: HasClockDomainCrossing) extends AnyVal {
    def crossIn (n: SramQInwardNode) (implicit valName: ValName) = SramQInwardClockCrossingHelper(valName.name, x, n)
    def crossOut(n: SramQOutwardNode)(implicit valName: ValName) = SramQOutwardClockCrossingHelper(valName.name, x, n)
    def cross(n: SramQInwardNode) (implicit valName: ValName) = crossIn(n)
    def cross(n: SramQOutwardNode)(implicit valName: ValName) = crossOut(n)
  }

  implicit class SramQResetDomainCrossing(private val x: HasResetDomainCrossing) extends AnyVal {
    def crossIn (n: SramQInwardNode) (implicit valName: ValName) = SramQInwardResetCrossingHelper(valName.name, x, n)
    def crossOut(n: SramQOutwardNode)(implicit valName: ValName) = SramQOutwardResetCrossingHelper(valName.name, x, n)
    def cross(n: SramQInwardNode) (implicit valName: ValName) = crossIn(n)
    def cross(n: SramQOutwardNode)(implicit valName: ValName) = crossOut(n)
  }
}
