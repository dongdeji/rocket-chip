// See LICENSE.SiFive for license details.

package freechips.rocketchip.amba.sramq

import chisel3._
import chisel3.internal.sourceinfo.SourceInfo
import freechips.rocketchip.config.{Parameters, Field}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util.AsyncQueueParams

//case object SramQMonitorBuilder extends Field[SramQMonitorArgs => SramQMonitorBase]

object SramQImp extends SimpleNodeImp[SramQMasterPortParameters, SramQSlavePortParameters, SramQEdgeParameters, SramQBundle]
{
  def edge(pd: SramQMasterPortParameters, pu: SramQSlavePortParameters, p: Parameters, sourceInfo: SourceInfo) = SramQEdgeParameters(pd, pu, p, sourceInfo)
  def bundle(e: SramQEdgeParameters) = SramQBundle(e.bundle)
  def render(e: SramQEdgeParameters) = RenderedEdge(colour = "#00ccff" /* bluish */, label  = (e.slave.beatBytes * 8).toString)

  override def mixO(pd: SramQMasterPortParameters, node: OutwardNode[SramQMasterPortParameters, SramQSlavePortParameters, SramQBundle]): SramQMasterPortParameters  =
   pd.copy(masters = pd.masters.map  { c => c.copy (nodePath = node +: c.nodePath) })
  override def mixI(pu: SramQSlavePortParameters, node: InwardNode[SramQMasterPortParameters, SramQSlavePortParameters, SramQBundle]): SramQSlavePortParameters =
   pu.copy(slaves  = pu.slaves.map { m => m.copy (nodePath = node +: m.nodePath) })
}

case class SramQMasterNode(portParams: Seq[SramQMasterPortParameters])(implicit valName: ValName) extends SourceNode(SramQImp)(portParams)
case class SramQSlaveNode(portParams: Seq[SramQSlavePortParameters])(implicit valName: ValName) extends SinkNode(SramQImp)(portParams)
case class SramQNexusNode(
  masterFn:       Seq[SramQMasterPortParameters] => SramQMasterPortParameters,
  slaveFn:        Seq[SramQSlavePortParameters]  => SramQSlavePortParameters)(
  implicit valName: ValName)
  extends NexusNode(SramQImp)(masterFn, slaveFn)
case class SramQAdapterNode(
  masterFn:  SramQMasterPortParameters => SramQMasterPortParameters = { m => m },
  slaveFn:   SramQSlavePortParameters  => SramQSlavePortParameters  = { s => s })(
  implicit valName: ValName)
  extends AdapterNode(SramQImp)(masterFn, slaveFn)
case class SramQIdentityNode()(implicit valName: ValName) extends IdentityNode(SramQImp)()

object SramQNameNode {
  def apply(name: ValName) = SramQIdentityNode()(name)
  def apply(name: Option[String]): SramQIdentityNode = apply((ValName(name.getOrElse("with_no_name"))))
  def apply(name: String): SramQIdentityNode = apply(Some(name))
}

