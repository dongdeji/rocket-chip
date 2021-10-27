// See LICENSE.SiFive for license details.

package freechips.rocketchip.amba.sramq

import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.prci.{ResetCrossingType, NoResetCrossing, StretchedResetCrossing}

trait SramQOutwardCrossingHelper {
  type HelperCrossingType <: CrossingType
  def apply(xing: HelperCrossingType)(implicit p: Parameters): SramQOutwardNode
}

trait SramQInwardCrossingHelper {
  type HelperCrossingType <: CrossingType
  def apply(xing: HelperCrossingType)(implicit p: Parameters): SramQInwardNode
}

case class SramQInwardClockCrossingHelper(name: String, scope: LazyScope, node: SramQInwardNode)
  extends SramQInwardCrossingHelper
{
  type HelperCrossingType = ClockCrossingType
  def apply(xing: ClockCrossingType = NoCrossing)(implicit p: Parameters): SramQInwardNode = {
    xing match {
      //case x: AsynchronousCrossing =>
      //  node :*=* scope { SramQAsyncCrossingSink(x.asSinkParams) :*=* SramQAsyncNameNode(name) } :*=* SramQAsyncNameNode(name) :*=* SramQAsyncCrossingSource(x.sourceSync)
      case RationalCrossing(direction) =>
        throw new IllegalArgumentException("SramQ Rational crossing unimplemented")
      case SynchronousCrossing(buffer) =>
        node :*=* scope { SramQBuffer(buffer) :*=* SramQNameNode(name) } :*=* SramQNameNode(name)
      //case CreditedCrossing(sourceDelay, sinkDelay) =>
      //  node :*=* scope { SramQCreditedSink(sinkDelay) :*=* SramQCreditedNameNode(name) } :*=* SramQCreditedNameNode(name) :*=* SramQCreditedSource(sourceDelay)
    }
  }
}

case class SramQInwardResetCrossingHelper(name: String, scope: LazyScope, node: SramQInwardNode)
  extends SramQInwardCrossingHelper
{
  type HelperCrossingType = ResetCrossingType
  def apply(xing: ResetCrossingType)(implicit p: Parameters): SramQInwardNode = {
    xing match {
      case _: NoResetCrossing => node
      case _: StretchedResetCrossing => throw new Exception("No ResetStretcher adapter for AXI$")
    }
  }
}

case class SramQOutwardClockCrossingHelper(name: String, scope: LazyScope, node: SramQOutwardNode)
  extends SramQOutwardCrossingHelper
{
  type HelperCrossingType = ClockCrossingType
  def apply(xing: ClockCrossingType = NoCrossing)(implicit p: Parameters): SramQOutwardNode = {
    xing match {
      //case x: AsynchronousCrossing =>
      //  SramQAsyncCrossingSink(x.asSinkParams) :*=* SramQAsyncNameNode(name) :*=* scope { SramQAsyncNameNode(name) :*=* SramQAsyncCrossingSource(x.sourceSync) } :*=* node
      case RationalCrossing(direction) =>
        throw new IllegalArgumentException("SramQ Rational crossing unimplemented")
      case SynchronousCrossing(buffer) =>
        SramQNameNode(name) :*=* scope { SramQNameNode(name) :*=* SramQBuffer(buffer) } :*=* node
      //case CreditedCrossing(sourceDelay, sinkDelay) =>
      //  SramQCreditedSink(sinkDelay) :*=* SramQCreditedNameNode(name) :*=* scope { SramQCreditedNameNode(name) :*=* SramQCreditedSource(sourceDelay) } :*=* node
    }
  }
}

case class SramQOutwardResetCrossingHelper(name: String, scope: LazyScope, node: SramQOutwardNode)
  extends SramQOutwardCrossingHelper
{
  type HelperCrossingType = ResetCrossingType
  def apply(xing: ResetCrossingType)(implicit p: Parameters): SramQOutwardNode = {
    xing match {
      case _: NoResetCrossing => node
      case _: StretchedResetCrossing => throw new Exception("No ResetStretcher adapter for AXI$")
    }
  }
}
