package lunaris.recipes.eval

import akka.stream.Materializer
import lunaris.io.ResourceConfig

case class LunRunContext(materializer: Materializer, resourceConfig: ResourceConfig)
