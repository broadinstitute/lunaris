package lunaris.streams.tools

trait Tool {
  def name: String

  def resultType: ResultType

  def params: Map[String, ResultType]
}
