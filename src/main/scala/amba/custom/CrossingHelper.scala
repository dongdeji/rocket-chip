// See LICENSE.SiFive for license details.

package freechips.rocketchip.amba.custom

import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.prci.{ResetCrossingType, NoResetCrossing, StretchedResetCrossing}

trait CustomOutwardCrossingHelper {
  type HelperCrossingType <: CrossingType
  def apply(xing: HelperCrossingType)(implicit p: Parameters): CustomOutwardNode
}

trait CustomInwardCrossingHelper {
  type HelperCrossingType <: CrossingType
  def apply(xing: HelperCrossingType)(implicit p: Parameters): CustomInwardNode
}

case class CustomInwardClockCrossingHelper(name: String, scope: LazyScope, node: CustomInwardNode)
  extends CustomInwardCrossingHelper
{
  type HelperCrossingType = ClockCrossingType
  def apply(xing: ClockCrossingType = NoCrossing)(implicit p: Parameters): CustomInwardNode = {
    xing match {
      //case x: AsynchronousCrossing =>
      //  node :*=* scope { CustomAsyncCrossingSink(x.asSinkParams) :*=* CustomAsyncNameNode(name) } :*=* CustomAsyncNameNode(name) :*=* CustomAsyncCrossingSource(x.sourceSync)
      case RationalCrossing(direction) =>
        throw new IllegalArgumentException("Custom Rational crossing unimplemented")
      case SynchronousCrossing(buffer) =>
        node :*=* scope { CustomBuffer(buffer) :*=* CustomNameNode(name) } :*=* CustomNameNode(name)
      //case CreditedCrossing(sourceDelay, sinkDelay) =>
      //  node :*=* scope { CustomCreditedSink(sinkDelay) :*=* CustomCreditedNameNode(name) } :*=* CustomCreditedNameNode(name) :*=* CustomCreditedSource(sourceDelay)
    }
  }
}

case class CustomInwardResetCrossingHelper(name: String, scope: LazyScope, node: CustomInwardNode)
  extends CustomInwardCrossingHelper
{
  type HelperCrossingType = ResetCrossingType
  def apply(xing: ResetCrossingType)(implicit p: Parameters): CustomInwardNode = {
    xing match {
      case _: NoResetCrossing => node
      case _: StretchedResetCrossing => throw new Exception("No ResetStretcher adapter for AXI$")
    }
  }
}

case class CustomOutwardClockCrossingHelper(name: String, scope: LazyScope, node: CustomOutwardNode)
  extends CustomOutwardCrossingHelper
{
  type HelperCrossingType = ClockCrossingType
  def apply(xing: ClockCrossingType = NoCrossing)(implicit p: Parameters): CustomOutwardNode = {
    xing match {
      //case x: AsynchronousCrossing =>
      //  CustomAsyncCrossingSink(x.asSinkParams) :*=* CustomAsyncNameNode(name) :*=* scope { CustomAsyncNameNode(name) :*=* CustomAsyncCrossingSource(x.sourceSync) } :*=* node
      case RationalCrossing(direction) =>
        throw new IllegalArgumentException("Custom Rational crossing unimplemented")
      case SynchronousCrossing(buffer) =>
        CustomNameNode(name) :*=* scope { CustomNameNode(name) :*=* CustomBuffer(buffer) } :*=* node
      //case CreditedCrossing(sourceDelay, sinkDelay) =>
      //  CustomCreditedSink(sinkDelay) :*=* CustomCreditedNameNode(name) :*=* scope { CustomCreditedNameNode(name) :*=* CustomCreditedSource(sourceDelay) } :*=* node
    }
  }
}

case class CustomOutwardResetCrossingHelper(name: String, scope: LazyScope, node: CustomOutwardNode)
  extends CustomOutwardCrossingHelper
{
  type HelperCrossingType = ResetCrossingType
  def apply(xing: ResetCrossingType)(implicit p: Parameters): CustomOutwardNode = {
    xing match {
      case _: NoResetCrossing => node
      case _: StretchedResetCrossing => throw new Exception("No ResetStretcher adapter for AXI$")
    }
  }
}
