package lunaris.streams.tools

import lunaris.streams.tools.Tool.Param
import lunaris.streams.values.LunType

trait Tool {
  def name: String
  def resultType: LunType
  def params: Seq[Param]
  def paramsByName: Map[String, Param] = params.map(param => (param.name, param)).toMap
}

object Tool {
  case class Param(name: String, lunType: LunType, isRef: Boolean, isRequired: Boolean)
}
