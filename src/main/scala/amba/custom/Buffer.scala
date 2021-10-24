// See LICENSE.SiFive for license details.

package freechips.rocketchip.amba.custom

/*import freechips.rocketchip.config._
import freechips.rocketchip.util._
import freechips.rocketchip.diplomacy._

class CustomBuffer(val params: BufferParams)(implicit p: Parameters) extends LazyModule
{
  val node = CustomAdapterNode()
  lazy val module = new LazyModuleImp(this) {
    (node.in zip node.out) foreach { case ((in, edgeIn), (out, edgeOut)) =>
      out :<>: params.irrevocable(in)
    }
  }
}

object CustomBuffer
{
  def apply(params: BufferParams = BufferParams.default)(implicit p: Parameters) = {
    val buffer = LazyModule(new CustomBuffer(params))
    buffer.node
  }
}
*/
