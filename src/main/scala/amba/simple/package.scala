// See LICENSE.SiFive for license details.

package freechips.rocketchip.amba

import freechips.rocketchip.diplomacy.{HasClockDomainCrossing, _}
import freechips.rocketchip.prci.{HasResetDomainCrossing}

package object simple
{
  type SimpleNode = SimpleNodeHandle[SimpleMasterPortParameters, SimpleSlavePortParameters, SimpleEdgeParameters, SimpleBundle]
  type SimpleOutwardNode = OutwardNodeHandle[SimpleMasterPortParameters, SimpleSlavePortParameters, SimpleEdgeParameters, SimpleBundle]
  type SimpleInwardNode = InwardNodeHandle[SimpleMasterPortParameters, SimpleSlavePortParameters, SimpleEdgeParameters, SimpleBundle]

  implicit class SimpleClockDomainCrossing(private val x: HasClockDomainCrossing) extends AnyVal {
    def crossIn (n: SimpleInwardNode) (implicit valName: ValName) = SimpleInwardClockCrossingHelper(valName.name, x, n)
    def crossOut(n: SimpleOutwardNode)(implicit valName: ValName) = SimpleOutwardClockCrossingHelper(valName.name, x, n)
    def cross(n: SimpleInwardNode) (implicit valName: ValName) = crossIn(n)
    def cross(n: SimpleOutwardNode)(implicit valName: ValName) = crossOut(n)
  }

  implicit class SimpleResetDomainCrossing(private val x: HasResetDomainCrossing) extends AnyVal {
    def crossIn (n: SimpleInwardNode) (implicit valName: ValName) = SimpleInwardResetCrossingHelper(valName.name, x, n)
    def crossOut(n: SimpleOutwardNode)(implicit valName: ValName) = SimpleOutwardResetCrossingHelper(valName.name, x, n)
    def cross(n: SimpleInwardNode) (implicit valName: ValName) = crossIn(n)
    def cross(n: SimpleOutwardNode)(implicit valName: ValName) = crossOut(n)
  }
}
