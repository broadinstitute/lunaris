package lunaris.io.bgz

import java.nio.ByteOrder
import java.nio.channels.ReadableByteChannel

import lunaris.io.{ByteBufferReader, ByteBufferRefiller}
import org.broadinstitute.yootilz.core.snag.Snag

case class BGZBlock(header: BGZHeader, footer: BGZFooter, unzippedData: BGZUnzippedData) {

}

object BGZBlock {
  val maxBlockSize: Int = 65536  //  Math.pow(2, 16).toInt, per BGZF specs

  def readBlock(readChannel: ReadableByteChannel): Either[Snag, BGZBlock] = {
    val refiller = ByteBufferRefiller(readChannel, maxBlockSize)
    refiller.buffer.order(ByteOrder.LITTLE_ENDIAN)
    val reader = ByteBufferReader(refiller)
    readBlock(reader)
  }

  def readThreeBlocks(readChannel: ReadableByteChannel): Either[Snag, (BGZBlock, BGZBlock, BGZBlock)] = {
    val refiller = ByteBufferRefiller(readChannel, maxBlockSize)
    refiller.buffer.order(ByteOrder.LITTLE_ENDIAN)
    val reader = ByteBufferReader(refiller)
    val snagOrBlock1 = readBlock(reader)
    println(snagOrBlock1)
    val snagOrBlock2 = readBlock(reader)
    println(snagOrBlock2)
    val snagOrBlock3 = readBlock(reader)
    println(snagOrBlock3)
    for {
      block1 <- snagOrBlock1
      block2 <- snagOrBlock2
      block3 <- snagOrBlock3
    } yield (block1, block2, block3)
  }

  def readBlock(reader: ByteBufferReader): Either[Snag, BGZBlock] = {
    val blockStartPos = reader.refiller.buffer.mark()
    for {
      header <- {
        BGZHeader.read(reader)
      }
      footer <- {
        reader.refiller.buffer.reset()
        reader.refiller.makeAvailable(header.blockSize)
        reader.refiller.buffer.mark()
        reader.refiller.buffer.position(reader.refiller.buffer.position() + header.blockSize - BGZFooter.nFooterBytes)
        BGZFooter.read(reader, header)
      }
      unzippedData <- {
        reader.refiller.buffer.reset()
        BGZUnzippedData.read(reader, header.blockSize)
      }
    } yield BGZBlock(header, footer, unzippedData)
  }
}