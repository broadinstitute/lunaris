package lunaris.streams

import org.broadinstitute.yootilz.core.snag.Snag

trait RecordProcessor[T] extends (Either[Snag, T] => Either[Snag, Option[T]]){

}

object RecordProcessor {
  def failOnFaultyRecord[T]: RecordProcessor[T] = {
    case Left(snag) => Left(snag)
    case Right(record) => Right(Some(record))
  }
  def ignoreFaultyRecords[T]: RecordProcessor[T] = {
    case Left(_) => Right(None)
    case Right(record) => Right(Some(record))
  }

  def printSnagsDropFaultyRecords[T]: RecordProcessor[T] = {
    case Left(snag) =>
      println(snag.message)
      Right(None)
    case Right(record) =>
      Right(Some(record))
  }

  def newFaultyRecordsLogger[T](): RecordProcessor[T] = new RecordProcessor[T] {
    var snags: Seq[Snag] = Vector.empty

    override def apply(snagOrRecord: Either[Snag, T]): Either[Snag, Option[T]] = {
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