// See LICENSE.SiFive for license details.

package freechips.rocketchip.amba.simple

import Chisel._
import chisel3.util.IrrevocableIO
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import scala.math.min


class SimpleBuffer(
  req: BufferParams,
  rsp: BufferParams)(implicit p: Parameters) extends LazyModule
{
  def this(x: BufferParams)(implicit p: Parameters) = this(x, x)
  def this()(implicit p: Parameters) = this(BufferParams.default)

  val node = SimpleAdapterNode(
    masterFn = { p => p },
    slaveFn  = { p => p.copy(minLatency = p.minLatency + req.latency + rsp.latency) })

  lazy val module = new LazyModuleImp(this) {
    def buffer[T <: Data](config: BufferParams, data: IrrevocableIO[T]): IrrevocableIO[T] = {
      if (config.isDefined) {
        Queue.irrevocable(data, config.depth, pipe=config.pipe, flow=config.flow)
      } else {
        data
      }
    }

    (node.in zip node.out) foreach { case ((in, edgeIn), (out, edgeOut)) =>
      out.req <> buffer(req, in.req)
      in.rsp <> buffer(rsp, out.rsp)
    }
  }
}

object SimpleBuffer
{
  def apply()(implicit p: Parameters): SimpleNode = apply(BufferParams.default)
  def apply(z: BufferParams)(implicit p: Parameters): SimpleNode = apply(z, z)
  def apply(req: BufferParams, rsp: BufferParams)(implicit p: Parameters): SimpleNode = 
  {
    val sramqbuf = LazyModule(new SimpleBuffer(req, rsp))
    sramqbuf.node
  }
}

