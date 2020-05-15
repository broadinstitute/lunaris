package lunaris.recipes.tools

import lunaris.io.ResourceConfig
import lunaris.recipes.tools.Tool.Param
import lunaris.recipes.values.LunType
import org.broadinstitute.yootilz.core.snag.Snag

trait Tool {
  def name: String = getClass.getSimpleName.filterNot(_ == '$')

  def resultType: LunType

  def params: Seq[Param]

  def paramsByName: Map[String, Param] = params.map(param => (param.name, param)).toMap

  def newToolInstance(args: Map[String, ToolCall.Arg]): Either[Snag, ToolInstance]

  def isFinal: Boolean
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
