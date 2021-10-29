// See LICENSE.SiFive for license details.

package freechips.rocketchip.amba

import freechips.rocketchip.diplomacy.{HasClockDomainCrossing, _}
import freechips.rocketchip.prci.{HasResetDomainCrossing}

package object custom
{
  type CustomNode = SimpleNodeHandle[CustomMasterPortParameters, CustomSlavePortParameters, CustomEdgeParameters, CustomBundle]
  type CustomOutwardNode = OutwardNodeHandle[CustomMasterPortParameters, CustomSlavePortParameters, CustomEdgeParameters, CustomBundle]
  type CustomInwardNode = InwardNodeHandle[CustomMasterPortParameters, CustomSlavePortParameters, CustomEdgeParameters, CustomBundle]

  implicit class CustomClockDomainCrossing(private val x: HasClockDomainCrossing) extends AnyVal {
    def crossIn (n: CustomInwardNode) (implicit valName: ValName) = CustomInwardClockCrossingHelper(valName.name, x, n)
    def crossOut(n: CustomOutwardNode)(implicit valName: ValName) = CustomOutwardClockCrossingHelper(valName.name, x, n)
    def cross(n: CustomInwardNode) (implicit valName: ValName) = crossIn(n)
    def cross(n: CustomOutwardNode)(implicit valName: ValName) = crossOut(n)
  }

  implicit class CustomResetDomainCrossing(private val x: HasResetDomainCrossing) extends AnyVal {
    def crossIn (n: CustomInwardNode) (implicit valName: ValName) = CustomInwardResetCrossingHelper(valName.name, x, n)
    def crossOut(n: CustomOutwardNode)(implicit valName: ValName) = CustomOutwardResetCrossingHelper(valName.name, x, n)
    def cross(n: CustomInwardNode) (implicit valName: ValName) = crossIn(n)
    def cross(n: CustomOutwardNode)(implicit valName: ValName) = crossOut(n)
  }
}
