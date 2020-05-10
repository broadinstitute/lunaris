package lunaris.streams

import org.broadinstitute.yootilz.core.snag.Snag

trait RecordProcessor extends (Either[Snag, Record] => Either[Snag, Option[Record]]){

}

object RecordProcessor {
  val failOnFaultyRecord: RecordProcessor = {
    case Left(snag) => Left(snag)
    case Right(record) => Right(Some(record))
  }
  val ignoreFaultyRecords: RecordProcessor = {
    case Left(snag) => Right(None)
    case Right(record) => Right(Some(record))
  }

  def newFaultyRecordsLogger(): RecordProcessor = new RecordProcessor {
    var snags: Seq[Snag] = Vector.empty

    override def apply(snagOrRecord: Either[Snag, Record]): Either[Snag, Option[Record]] = {
      snagOrRecord match {
        case Left(snag) =>
          snags :+= snag
          Right(None)
        case Right(record) =>
          Right(Some(record))
      }
    }
  }

}