package lunaris.recipes.eval

import lunaris.genomics.Region
import lunaris.genomics.utils.RegionConsolidator
import lunaris.io.request.Request
import lunaris.recipes.Recipe

case class LunCompileContext(regions: Map[String, Seq[Region]]) {

}

object LunCompileContext {
  def fromRequest(request: Request): LunCompileContext =
    LunCompileContext(RegionConsolidator.consolidateMap(request.regions))
}