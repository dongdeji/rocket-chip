// See LICENSE.SiFive for license details.

package freechips.rocketchip.amba.sramq

import Chisel._
import chisel3.util.IrrevocableIO
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import scala.math.min


class SramQBuffer(
  enqreq: BufferParams,
  enqrsp: BufferParams,
  deqreq: BufferParams,
  deqrsp: BufferParams)(implicit p: Parameters) extends LazyModule
{
  def this(enq: BufferParams, deq: BufferParams)(implicit p: Parameters) = this(enq, enq, deq, deq)
  def this(x: BufferParams)(implicit p: Parameters) = this(x, x, x, x)
  def this()(implicit p: Parameters) = this(BufferParams.default)

  val node = SramQAdapterNode(
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

object SramQBuffer
{
  def apply()(implicit p: Parameters): SramQNode = apply(BufferParams.default)
  def apply(z: BufferParams)(implicit p: Parameters): SramQNode = apply(z, z)
  def apply(enq: BufferParams, deq: BufferParams)(implicit p: Parameters): SramQNode = apply(enq, enq, deq, deq)
  def apply(enqreq: BufferParams, enqrsp: BufferParams, deqreq: BufferParams, deqrsp: BufferParams)(implicit p: Parameters): SramQNode = 
  {
    val sramqbuf = LazyModule(new SramQBuffer(enqreq, enqrsp, deqreq, deqrsp))
    sramqbuf.node
  }
}

