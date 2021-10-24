// See LICENSE.SiFive for license details.

package freechips.rocketchip.amba.custom

import chisel3.internal.sourceinfo.SourceInfo
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._

object CustomImp extends SimpleNodeImp[CustomMasterPortParameters, CustomSlavePortParameters, CustomEdgeParameters, CustomBundle]
{
  def edge(pd: CustomMasterPortParameters, pu: CustomSlavePortParameters, p: Parameters, sourceInfo: SourceInfo) = CustomEdgeParameters.v1(pd, pu, p, sourceInfo)
  def bundle(e: CustomEdgeParameters) = CustomBundle(e.bundle)
  def render(e: CustomEdgeParameters) = RenderedEdge(colour = "#00ccff" /* bluish */, (e.beatBytes * 8).toString)

  override def mixO(pd: CustomMasterPortParameters, node: OutwardNode[CustomMasterPortParameters, CustomSlavePortParameters, CustomBundle]): CustomMasterPortParameters  =
    pd.v1copy(masters = pd.masters.map  { c => c.v1copy (nodePath = node +: c.nodePath) })
  override def mixI(pu: CustomSlavePortParameters, node: InwardNode[CustomMasterPortParameters, CustomSlavePortParameters, CustomBundle]): CustomSlavePortParameters =
    pu.v1copy(slaves  = pu.slaves.map { m => m.v1copy (nodePath = node +: m.nodePath) })
}

case class CustomMasterNode(portParams: Seq[CustomMasterPortParameters])(implicit valName: ValName) extends SourceNode(CustomImp)(portParams)
case class CustomSlaveNode(portParams: Seq[CustomSlavePortParameters])(implicit valName: ValName) extends SinkNode(CustomImp)(portParams)
case class CustomNexusNode(
  masterFn:       Seq[CustomMasterPortParameters] => CustomMasterPortParameters,
  slaveFn:        Seq[CustomSlavePortParameters]  => CustomSlavePortParameters)(
  implicit valName: ValName)
  extends NexusNode(CustomImp)(masterFn, slaveFn)

case class CustomAdapterNode(
  masterFn:  CustomMasterPortParameters => CustomMasterPortParameters = { m => m },
  slaveFn:   CustomSlavePortParameters  => CustomSlavePortParameters  = { s => s })(
  implicit valName: ValName)
  extends AdapterNode(CustomImp)(masterFn, slaveFn)
case class CustomIdentityNode()(implicit valName: ValName) extends IdentityNode(CustomImp)()
