package lunaris.app

import java.io.InputStream
import java.nio.file.Files

import htsjdk.samtools.util.BlockCompressedInputStream
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
    val rawIndexInputStream: InputStream = Files.newInputStream(dataSourceWithIndex.index)
    val unzippedIndexInputStream = new BlockCompressedInputStream(rawIndexInputStream)
    val tabixIndex: TabixIndex = new TabixIndex(unzippedIndexInputStream)
    val chromCol = 0
    val posCol = 1
    val featureReader =
      AbstractFeatureReader.getFeatureReader(featureResource, RecordCodec(chromCol, posCol), tabixIndex)
//    val header = featureReader.getHeader
//    println(header)
//    val recordIterator = featureReader.query("1", 1, 100000)
    val recordIterator = featureReader.iterator()
    var count: Int = 0
    while (count < 10 && recordIterator.hasNext) {
      val record = recordIterator.next()
      println(record)
      count += 1
    }
  }

}
