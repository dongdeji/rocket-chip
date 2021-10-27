// See LICENSE.SiFive for license details.

package freechips.rocketchip.amba.custom

import Chisel._
import chisel3.util.IrrevocableIO
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import scala.math.min


class CustomBuffer(
  enqreq: BufferParams,
  enqrsp: BufferParams,
  deqreq: BufferParams,
  deqrsp: BufferParams)(implicit p: Parameters) extends LazyModule
{
  def this(enq: BufferParams, deq: BufferParams)(implicit p: Parameters) = this(enq, enq, deq, deq)
  def this(x: BufferParams)(implicit p: Parameters) = this(x, x, x, x)
  def this()(implicit p: Parameters) = this(BufferParams.default)

  val node = CustomAdapterNode(
    masterFn = { p => p },
    slaveFn  = { p => p.copy(minLatency = p.minLatency + 
                                            min(enqreq.latency, deqreq.latency) + 
                                              min(enqrsp.latency, deqrsp.latency)) })

  lazy val module = new LazyModuleImp(this) {
    def buffer[T <: Data](config: BufferParams, data: IrrevocableIO[T]): IrrevocableIO[T] = {
      if (config.isDefined) {
        Queue.irrevocable(data, config.depth, pipe=config.pipe, flow=config.flow)
      } else {
        data
      }
    }

    (node.in zip node.out) foreach { case ((in, edgeIn), (out, edgeOut)) =>
      out.enqreq <> buffer(enqreq, in.enqreq)
      in.enqrsp <> buffer(enqrsp, out.enqrsp)
      out.deqreq <> buffer(deqreq, in.deqreq)
      in.deqrsp <> buffer(deqrsp, out.deqrsp)
    }
  }
}

object CustomBuffer
{
  def apply()(implicit p: Parameters): CustomNode = apply(BufferParams.default)
  def apply(z: BufferParams)(implicit p: Parameters): CustomNode = apply(z, z)
  def apply(enq: BufferParams, deq: BufferParams)(implicit p: Parameters): CustomNode = apply(enq, enq, deq, deq)
  def apply(enqreq: BufferParams, enqrsp: BufferParams, deqreq: BufferParams, deqrsp: BufferParams)(implicit p: Parameters): CustomNode = 
  {
    val custombuf = LazyModule(new CustomBuffer(enqreq, enqrsp, deqreq, deqrsp))
    custombuf.node
  }
}

