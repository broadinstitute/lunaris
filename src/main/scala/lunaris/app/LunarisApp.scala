package lunaris.app

import lunaris.data.DataSources
import lunaris.io.InputId
import lunaris.io.query.RecordExtractor
import lunaris.io.query.RecordExtractor.HeaderAndRecordEtor
import lunaris.io.request.RequestJson
import lunaris.streams.RecordProcessor
import lunaris.streams.tools.ToolsChecker
import lunaris.utils.{DebugUtils, IOUtils}

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
          ToolsChecker.checkTools(request.tools) match {
            case Left(snag) => DebugUtils.printSnag("Problem with tools!", snag)
            case Right(()) => println("The tools are alright.")
          }
          val data = DataSources.T2D.variants
          val regionsBySequence = request.regions
          println(regionsBySequence)
          println("Now going to extract records")
          RecordExtractor.extract(data, regionsBySequence,
            RecordProcessor.failOnFaultyRecord).useUp {
            case Left(snag) =>
              DebugUtils.printSnag("Problem!", snag)
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
