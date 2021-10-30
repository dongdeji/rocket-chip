// See LICENSE.SiFive for license details.

package freechips.rocketchip.amba.sramq

import Chisel._
import chisel3.util.IrrevocableIO
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import scala.math.min


class SramQBuffer(
  wirte_req: BufferParams,
  wirte_rsp: BufferParams,
  read_req: BufferParams,
  read_rsp: BufferParams)(implicit p: Parameters) extends LazyModule
{
  def this(enq: BufferParams, deq: BufferParams)(implicit p: Parameters) = this(enq, enq, deq, deq)
  def this(x: BufferParams)(implicit p: Parameters) = this(x, x, x, x)
  def this()(implicit p: Parameters) = this(BufferParams.default)

  val node = SramQAdapterNode(
    masterFn = { p => p },
    slaveFn  = { p => p.copy(minLatency = p.minLatency + 
                                            min(wirte_req.latency, read_req.latency) + 
                                              min(wirte_rsp.latency, read_rsp.latency)) })

  lazy val module = new LazyModuleImp(this) {
    def buffer[T <: Data](config: BufferParams, data: IrrevocableIO[T]): IrrevocableIO[T] = {
      if (config.isDefined) {
        Queue.irrevocable(data, config.depth, pipe=config.pipe, flow=config.flow)
      } else {
        data
      }
    }

    (node.in zip node.out) foreach { case ((in, edgeIn), (out, edgeOut)) =>
      out.wirte_req <> buffer(wirte_req, in.wirte_req)
      in.wirte_rsp <> buffer(wirte_rsp, out.wirte_rsp)
      out.read_req <> buffer(read_req, in.read_req)
      in.read_rsp <> buffer(read_rsp, out.read_rsp)
    }
  }
}

object SramQBuffer
{
  def apply()(implicit p: Parameters): SramQNode = apply(BufferParams.default)
  def apply(z: BufferParams)(implicit p: Parameters): SramQNode = apply(z, z)
  def apply(enq: BufferParams, deq: BufferParams)(implicit p: Parameters): SramQNode = apply(enq, enq, deq, deq)
  def apply(wirte_req: BufferParams, wirte_rsp: BufferParams, read_req: BufferParams, read_rsp: BufferParams)(implicit p: Parameters): SramQNode = 
  {
    val sramqbuf = LazyModule(new SramQBuffer(wirte_req, wirte_rsp, read_req, read_rsp))
    sramqbuf.node
  }
}

