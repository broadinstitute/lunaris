package lunaris.app

import java.io.InputStream
import java.nio.file.Files

import htsjdk.samtools.util.IOUtil
import htsjdk.tribble.AbstractFeatureReader
import htsjdk.tribble.index.tabix.TabixIndex
import lunaris.stream.RecordCodec

object LunarisApp {

  def main(args: Array[String]): Unit = {
    val pathUri = "gs://some/some"
    val path = IOUtil.getPath(pathUri)
    println("Hello, world!")
    println(path.toUri)
    val featureResource = "gs://fc-6fe31e1f-2c36-411c-bf23-60656d621184/data/sim/sim.tsv.gz"
    val indexResource = "gs://fc-6fe31e1f-2c36-411c-bf23-60656d621184/data/sim/sim.tsv.gz.tbi"
    val indexInputStream: InputStream = Files.newInputStream(IOUtil.getPath(indexResource))
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
