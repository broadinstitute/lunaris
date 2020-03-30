package lunaris.app

import java.io.InputStream
import java.nio.file.Files

import htsjdk.samtools.seekablestream.SeekableStreamFactory
import htsjdk.samtools.util.{BlockCompressedInputStream, IOUtil}
import htsjdk.tribble.AbstractFeatureReader
import htsjdk.tribble.index.tabix.TabixIndex
import lunaris.data.DataSources
import lunaris.stream.RecordCodec

object LunarisApp {

  def main(args: Array[String]): Unit = {
    //    val dataSourceWithIndex = DataSources.simDataOnTerra
    val dataSourceWithIndex = DataSources.simDataInPublicBucket
    val featureResource = dataSourceWithIndex.dataSource.toUri.toString
    println(featureResource)
    val chromCol = 0
    val posCol = 1
    val featureReader =
      AbstractFeatureReader.getFeatureReader(featureResource, dataSourceWithIndex.index.toUri.toString, RecordCodec(chromCol, posCol), true, null, null)
//    val header = featureReader.getHeader
//    println(header)
    val recordIterator = featureReader.query("8", 51844707, 67281173)
 //   val recordIterator = featureReader.iterator()
    var count: Int = 0
    while (count < 10 && recordIterator.hasNext) {
      val record = recordIterator.next()
      println(record)
      count += 1
    }
  }

}
