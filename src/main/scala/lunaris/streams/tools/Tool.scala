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

  sealed trait Param {
    def name: String

    def lunType: LunType

    def isRequired: Boolean
  }

  case class ValueParam(name: String, lunType: LunType.PrimitiveType, isRequired: Boolean) extends Param

  case class RefParam(name: String, lunType: LunType, isRequired: Boolean) extends Param

}
