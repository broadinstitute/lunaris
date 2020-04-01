package lunaris.io.bgz

import java.nio.ByteOrder
import java.nio.channels.ReadableByteChannel

import lunaris.io.ByteBufferRefiller
import org.broadinstitute.yootilz.core.snag.Snag

case class BGZBlock(header: BGZHeader, footer: BGZFooter, unzippedData: BGZUnzippedData) {

}

object BGZBlock {
  val maxBlockSize: Int = 65536  //  Math.pow(2, 16).toInt, per BGZF specs

  def read(readChannel: ReadableByteChannel): Either[Snag, BGZBlock] = {
    val refiller = ByteBufferRefiller(readChannel, maxBlockSize)
    refiller.buffer.order(ByteOrder.LITTLE_ENDIAN)
    for {
      header <- BGZHeader.read(refiller)
      footer <- BGZFooter.read(refiller, header)
      unzippedData <- BGZUnzippedData.read(refiller, header.blockSize)
    } yield BGZBlock(header, footer, unzippedData)
  }
}