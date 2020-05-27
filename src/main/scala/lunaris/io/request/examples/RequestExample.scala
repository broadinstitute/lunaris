package lunaris.io.request.examples

import lunaris.io.request.Request
import lunaris.recipes.values.LunValue.PrimitiveValue.FileValue

trait RequestExample {
  def idBase: String
  def id: String = "request" + idBase
  def outputFile: FileValue = FileValue(s"examples/responses/response$idBase.json")

  def request: Request
}
