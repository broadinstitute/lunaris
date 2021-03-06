package lunaris.recipes.eval

import lunaris.genomics.Region
import lunaris.io.request.Request

case class LunCompileContext(regions: Map[String, Seq[Region]]) {

}

object LunCompileContext {
  def fromRequest(request: Request): LunCompileContext =
    LunCompileContext(request.regions)
}