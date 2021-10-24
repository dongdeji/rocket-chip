// See LICENSE.SiFive for license details.

package freechips.rocketchip.amba

import freechips.rocketchip.diplomacy._

package object custom
{
  type CustomInwardNode = InwardNodeHandle[CustomMasterPortParameters, CustomSlavePortParameters, CustomEdgeParameters, CustomBundle]
  type CustomOutwardNode = OutwardNodeHandle[CustomMasterPortParameters, CustomSlavePortParameters, CustomEdgeParameters, CustomBundle]
  type CustomNode = NodeHandle[CustomMasterPortParameters, CustomSlavePortParameters, CustomEdgeParameters, CustomBundle, CustomMasterPortParameters, CustomSlavePortParameters, CustomEdgeParameters, CustomBundle]
}
