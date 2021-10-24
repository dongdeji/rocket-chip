// See LICENSE.SiFive for license details.

package freechips.rocketchip.amba.custom

import Chisel._
import chisel3.util.IrrevocableIO
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import scala.math.min


class CustomBuffer(
  a: BufferParams,
  b: BufferParams)(implicit p: Parameters) extends LazyModule
{
  def this(x: BufferParams)(implicit p: Parameters) = this(x, x)
  def this()(implicit p: Parameters) = this(BufferParams.default)

  val node = CustomAdapterNode(
    masterFn = { p => p },
    slaveFn  = { p => p.copy(minLatency = p.minLatency + a.latency + b.latency) })

  lazy val module = new LazyModuleImp(this) {
    def buffer[T <: Data](config: BufferParams, data: IrrevocableIO[T]): IrrevocableIO[T] = {
      if (config.isDefined) {
        Queue.irrevocable(data, config.depth, pipe=config.pipe, flow=config.flow)
      } else {
        data
      }
    }

    (node.in zip node.out) foreach { case ((in, edgeIn), (out, edgeOut)) =>
      out.a <> buffer(a, in.a)
      in .b <> buffer(b, out.b)
    }
  }
}

object CustomBuffer
{
  def apply()(implicit p: Parameters): CustomNode = apply(BufferParams.default)
  def apply(z: BufferParams)(implicit p: Parameters): CustomNode = apply(z, z)
  def apply(a: BufferParams, b: BufferParams)(implicit p: Parameters): CustomNode =
  {
    val custombuf = LazyModule(new CustomBuffer(a, b))
    custombuf.node
  }
}