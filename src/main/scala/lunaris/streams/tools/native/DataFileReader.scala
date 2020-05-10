package lunaris.streams.tools.native

import lunaris.streams.tools.{ResultType, Tool}

object DataFileReader extends Tool {
  override def name: String = "DataFileReader"

  override def resultType: ResultType = ResultType.StreamOfRecords

  override def params: Map[String, ResultType] = ???
}
