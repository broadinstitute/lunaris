package lunaris.app

import java.io.InputStream
import java.nio.file.Files

import htsjdk.tribble.AbstractFeatureReader
import htsjdk.tribble.index.tabix.TabixIndex
import lunaris.data.DataSources
import lunaris.stream.RecordCodec

object LunarisApp {

  def main(args: Array[String]): Unit = {
    //    val dataSourceWithIndex = DataSources.simDataOnTerra
        val dataSourceWithIndex = DataSources.simDataOnOliversOldLaptop
    val featureResource = dataSourceWithIndex.dataSource.toUri.toString
    println(featureResource)
    val indexInputStream: InputStream = Files.newInputStream(dataSourceWithIndex.index)
    val tabixIndex: TabixIndex = new TabixIndex(indexInputStream)
    val featureReader = AbstractFeatureReader.getFeatureReader(featureResource, RecordCodec, tabixIndex)
    val header = featureReader.getHeader
    println(header)
    val recordIterator = featureReader.query("1", 1, 100000)
    while (recordIterator.hasNext) {
      val record = recordIterator.next()
      println(record)
    }

  }

}
