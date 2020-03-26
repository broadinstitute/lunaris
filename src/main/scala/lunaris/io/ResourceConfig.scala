package lunaris.io

case class ResourceConfig(keyFileOpt: Option[InputId], gcpProjectOpt: Option[String])

object ResourceConfig {
  def empty: ResourceConfig = ResourceConfig(None, None)
}
