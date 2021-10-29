// See LICENSE.SiFive for license details.

package freechips.rocketchip.amba.simple

import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.prci.{ResetCrossingType, NoResetCrossing, StretchedResetCrossing}

trait SimpleOutwardCrossingHelper {
  type HelperCrossingType <: CrossingType
  def apply(xing: HelperCrossingType)(implicit p: Parameters): SimpleOutwardNode
}

trait SimpleInwardCrossingHelper {
  type HelperCrossingType <: CrossingType
  def apply(xing: HelperCrossingType)(implicit p: Parameters): SimpleInwardNode
}

case class SimpleInwardClockCrossingHelper(name: String, scope: LazyScope, node: SimpleInwardNode)
  extends SimpleInwardCrossingHelper
{
  type HelperCrossingType = ClockCrossingType
  def apply(xing: ClockCrossingType = NoCrossing)(implicit p: Parameters): SimpleInwardNode = {
    xing match {
      //case x: AsynchronousCrossing =>
      //  node :*=* scope { SimpleAsyncCrossingSink(x.asSinkParams) :*=* SimpleAsyncNameNode(name) } :*=* SimpleAsyncNameNode(name) :*=* SimpleAsyncCrossingSource(x.sourceSync)
      case RationalCrossing(direction) =>
        throw new IllegalArgumentException("Simple Rational crossing unimplemented")
      case SynchronousCrossing(buffer) =>
        node :*=* scope { SimpleBuffer(buffer) :*=* SimpleNameNode(name) } :*=* SimpleNameNode(name)
      //case CreditedCrossing(sourceDelay, sinkDelay) =>
      //  node :*=* scope { SimpleCreditedSink(sinkDelay) :*=* SimpleCreditedNameNode(name) } :*=* SimpleCreditedNameNode(name) :*=* SimpleCreditedSource(sourceDelay)
    }
  }
}

case class SimpleInwardResetCrossingHelper(name: String, scope: LazyScope, node: SimpleInwardNode)
  extends SimpleInwardCrossingHelper
{
  type HelperCrossingType = ResetCrossingType
  def apply(xing: ResetCrossingType)(implicit p: Parameters): SimpleInwardNode = {
    xing match {
      case _: NoResetCrossing => node
      case _: StretchedResetCrossing => throw new Exception("No ResetStretcher adapter for AXI$")
    }
  }
}

case class SimpleOutwardClockCrossingHelper(name: String, scope: LazyScope, node: SimpleOutwardNode)
  extends SimpleOutwardCrossingHelper
{
  type HelperCrossingType = ClockCrossingType
  def apply(xing: ClockCrossingType = NoCrossing)(implicit p: Parameters): SimpleOutwardNode = {
    xing match {
      //case x: AsynchronousCrossing =>
      //  SimpleAsyncCrossingSink(x.asSinkParams) :*=* SimpleAsyncNameNode(name) :*=* scope { SimpleAsyncNameNode(name) :*=* SimpleAsyncCrossingSource(x.sourceSync) } :*=* node
      case RationalCrossing(direction) =>
        throw new IllegalArgumentException("Simple Rational crossing unimplemented")
      case SynchronousCrossing(buffer) =>
        SimpleNameNode(name) :*=* scope { SimpleNameNode(name) :*=* SimpleBuffer(buffer) } :*=* node
      //case CreditedCrossing(sourceDelay, sinkDelay) =>
      //  SimpleCreditedSink(sinkDelay) :*=* SimpleCreditedNameNode(name) :*=* scope { SimpleCreditedNameNode(name) :*=* SimpleCreditedSource(sourceDelay) } :*=* node
    }
  }
}

case class SimpleOutwardResetCrossingHelper(name: String, scope: LazyScope, node: SimpleOutwardNode)
  extends SimpleOutwardCrossingHelper
{
  type HelperCrossingType = ResetCrossingType
  def apply(xing: ResetCrossingType)(implicit p: Parameters): SimpleOutwardNode = {
    xing match {
      case _: NoResetCrossing => node
      case _: StretchedResetCrossing => throw new Exception("No ResetStretcher adapter for AXI$")
    }
  }
}
