package lunaris.app

import lunaris.data.DataSources
import lunaris.genomics.{Chromosome, Region, Regions}
import lunaris.io.query.RecordExtractor

object LunarisApp {
  def main(args: Array[String]): Unit = {
    val usingLocalSimulatedDataOnOliversLaptop: Boolean = true
    val dataSourceWithIndex = if (usingLocalSimulatedDataOnOliversLaptop)
      DataSources.simDataOnOliversOldLaptop
    else
      DataSources.simDataOnTerra
    val regions = Regions(Map(Chromosome(1) -> Seq(Region(1, 100000))))
    val recordEitherator = RecordExtractor.extract(dataSourceWithIndex, regions)
    recordEitherator.foreach { record =>
      println(record)
    }
  }
}
