package lunaris.app

import lunaris.data.DataSources
import lunaris.io.InputId
import lunaris.io.query.RecordExtractor
import lunaris.io.query.RecordExtractor.HeaderAndRecordEtor
import lunaris.io.request.{RequestExamples, RequestJson}
import lunaris.stream.RecordProcessor
import lunaris.utils.IOUtils

object LunarisApp {
  def main(args: Array[String]): Unit = {
    if (args.length > 0) {
      val input = InputId(args(0))
      val snagOrRequest = for {
        requestString <-
          input.newReadChannelDisposable().useUp { readChannel =>
            IOUtils.readStringFromChannel(readChannel)
          }
        request <- RequestJson.parse(requestString)
      } yield request
      snagOrRequest match {
        case Left(snag) =>
          println(snag.message)
        case Right(request) =>
          val usingLocalSimulatedDataOnOliversLaptop: Boolean = false
          val data = DataSources.T2D.variants
          val regionsBySequence = request.regions
          println(regionsBySequence)
          println("Now going to extract records")
          RecordExtractor.extract(data, regionsBySequence,
            RecordProcessor.failOnFaultyRecord).useUp {
            case Left(snag) =>
              println("Problem!")
              println(snag.message)
              println(snag.report)
            case Right(HeaderAndRecordEtor(header, recordEtor)) =>
              println(header.asString)
              println(recordEtor.foreach(record => println(record.asString)))
              println("Done!")
          }
      }
    } else {
      println("Error: no input file was provided.")
    }
  }
}
