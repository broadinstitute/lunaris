package lunaris.streams.tools

trait ResultType {
  def canBeAssignedFrom(oResultType: ResultType): Boolean
}

object ResultType {

  object StreamOfRecords extends ResultType {
    override def canBeAssignedFrom(oResultType: ResultType): Boolean = oResultType == StreamOfRecords
  }

  object Unit extends ResultType {
    override def canBeAssignedFrom(oResultType: ResultType): Boolean = true
  }

}
