package lunaris.app

import lunaris.data.DataSources
import lunaris.genomics.Region
import lunaris.io.BedReader
import lunaris.io.query.RecordExtractor
import lunaris.io.query.RecordExtractor.HeaderAndRecordEtor
import lunaris.stream.RecordProcessor

object LunarisApp {
  def main(args: Array[String]): Unit = {
    val usingLocalSimulatedDataOnOliversLaptop: Boolean = true
    val dataSources = if (usingLocalSimulatedDataOnOliversLaptop)
      DataSources.simDataOnOliversOldLaptop
    else
      DataSources.simDataOnTerra
    BedReader.read(dataSources.bed, RecordProcessor.failOnFaultyRecord) match {
      case Left(snag) =>
        println("Problem!")
        println(snag.message)
        println(snag.report)
      case Right(regionsBySequence) =>
        println("Now going to extract records")
        RecordExtractor.extract(dataSources.data, regionsBySequence,
          RecordProcessor.failOnFaultyRecord).useUp {
          case Left(snag) =>
            println("Problem!")
            println(snag.message)
            println(snag.report)
          case Right(HeaderAndRecordEtor(header, recordEtor)) =>
            println(header.asString)
            recordEtor.foreach(record => println(record.asString))
        }
    }
    println("Done!")
  }
}
