// See LICENSE.SiFive for license details.

package freechips.rocketchip.amba.simple

import chisel3._
import chisel3.internal.sourceinfo.SourceInfo
import freechips.rocketchip.config.{Parameters, Field}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util.AsyncQueueParams

//case object SimpleMonitorBuilder extends Field[SimpleMonitorArgs => SimpleMonitorBase]

object SimpleImp extends SimpleNodeImp[SimpleMasterPortParameters, SimpleSlavePortParameters, SimpleEdgeParameters, SimpleBundle]
{
  def edge(pd: SimpleMasterPortParameters, pu: SimpleSlavePortParameters, p: Parameters, sourceInfo: SourceInfo) = SimpleEdgeParameters(pd, pu, p, sourceInfo)
  def bundle(e: SimpleEdgeParameters) = SimpleBundle(e.bundle)
  def render(e: SimpleEdgeParameters) = RenderedEdge(colour = "#00ccff" /* bluish */, label  = (e.slave.beatBytes * 8).toString)

  override def mixO(pd: SimpleMasterPortParameters, node: OutwardNode[SimpleMasterPortParameters, SimpleSlavePortParameters, SimpleBundle]): SimpleMasterPortParameters  =
   pd.copy(masters = pd.masters.map  { c => c.copy (nodePath = node +: c.nodePath) })
  override def mixI(pu: SimpleSlavePortParameters, node: InwardNode[SimpleMasterPortParameters, SimpleSlavePortParameters, SimpleBundle]): SimpleSlavePortParameters =
   pu.copy(slaves  = pu.slaves.map { m => m.copy (nodePath = node +: m.nodePath) })
}

case class SimpleMasterNode(portParams: Seq[SimpleMasterPortParameters])(implicit valName: ValName) extends SourceNode(SimpleImp)(portParams)
case class SimpleSlaveNode(portParams: Seq[SimpleSlavePortParameters])(implicit valName: ValName) extends SinkNode(SimpleImp)(portParams)
case class SimpleNexusNode(
  masterFn:       Seq[SimpleMasterPortParameters] => SimpleMasterPortParameters,
  slaveFn:        Seq[SimpleSlavePortParameters]  => SimpleSlavePortParameters)(
  implicit valName: ValName)
  extends NexusNode(SimpleImp)(masterFn, slaveFn)
case class SimpleAdapterNode(
  masterFn:  SimpleMasterPortParameters => SimpleMasterPortParameters = { m => m },
  slaveFn:   SimpleSlavePortParameters  => SimpleSlavePortParameters  = { s => s })(
  implicit valName: ValName)
  extends AdapterNode(SimpleImp)(masterFn, slaveFn)
case class SimpleIdentityNode()(implicit valName: ValName) extends IdentityNode(SimpleImp)()

object SimpleNameNode {
  def apply(name: ValName) = SimpleIdentityNode()(name)
  def apply(name: Option[String]): SimpleIdentityNode = apply((ValName(name.getOrElse("with_no_name"))))
  def apply(name: String): SimpleIdentityNode = apply(Some(name))
}

