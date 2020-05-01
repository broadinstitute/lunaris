package lunaris.app

import lunaris.data.DataSources
import lunaris.genomics.Region
import lunaris.io.query.RecordExtractor
import lunaris.io.query.RecordExtractor.{HeaderAndRecordEtor, ParsedRecordHandler}

object LunarisApp {
  def main(args: Array[String]): Unit = {
    val usingLocalSimulatedDataOnOliversLaptop: Boolean = true
    val dataSourceWithIndex = if (usingLocalSimulatedDataOnOliversLaptop)
      DataSources.simDataOnOliversOldLaptop
    else
      DataSources.simDataOnTerra
    val regionsBySequence = Map(
      "1" -> Seq(Region(100000, 200000), Region(100000, 200001)),
      "5" -> Seq(Region(200000, 300000)),
      "7" -> Seq(Region(0, 200000))
    )
    println("Now going to extract records")
    RecordExtractor.extract(dataSourceWithIndex, regionsBySequence,
      ParsedRecordHandler.failOnFaultyRecord).useUp {
      case Left(snag) =>
        println("Problem!")
        println(snag.message)
        println(snag.report)
      case Right(HeaderAndRecordEtor(header, recordEtor)) =>
        println(header.asString)
        recordEtor.foreach(record => println(record.asString))
    }
    println("Done!")
  }
}
