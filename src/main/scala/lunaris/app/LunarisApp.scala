package lunaris.app

import lunaris.data.DataSources
import lunaris.genomics.Region
import lunaris.io.query.RecordExtractor

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
    val recordEitherator = RecordExtractor.extract(dataSourceWithIndex, regionsBySequence)
    recordEitherator.foreach { record =>
      println(record)
    }
  }
}
