package lunaris.io.bgz

import java.nio.ByteOrder
import java.nio.channels.ReadableByteChannel

import lunaris.io.{ByteBufferReader, ByteBufferRefiller}
import lunaris.utils.{Eitherator, ReadableByteChannelUtils}
import org.broadinstitute.yootilz.core.snag.Snag

case class BGZBlock(header: BGZHeader, footer: BGZFooter, unzippedData: BGZUnzippedData) {
  def isEOFMarker: Boolean = unzippedData.bytes.length == 0  // per BGZF specs, EOF block with empty payload
}

object BGZBlock {
  val maxBlockSize: Int = 65536  //  Math.pow(2, 16).toInt, per BGZF specs

  def readBlock(readChannel: ReadableByteChannel): Either[Snag, BGZBlock] = {
    val refiller = ByteBufferRefiller(readChannel, maxBlockSize)
    refiller.buffer.order(ByteOrder.LITTLE_ENDIAN)
    val reader = ByteBufferReader(refiller)
    readBlock(reader)
  }

  class BlockEitherator(reader: ByteBufferReader, startPos: Long) extends Eitherator[BGZBlock] {
    var haveReadEOFBlock: Boolean = false
    var nextBlockStart: Long = startPos
    override def next(): Either[Snag, Option[BGZBlock]] = {
      if(haveReadEOFBlock) {
        Right(None)
      } else {
        readBlock(reader) match {
          case Left(snag) => Left(Snag("Could not read next block", snag))
          case Right(block) =>
            nextBlockStart += block.header.bsize.toPositiveLong
            if(block.isEOFMarker) {
              haveReadEOFBlock = true
              Right(None)
            } else {
              Right(Some(block))
            }
        }
      }
    }
    def skipToOffset(offsetOfBlock: Long): Unit = {
      ???
    }
  }

  def newBlockEitherator(readChannel: ReadableByteChannel, pos: Long = 0): BlockEitherator  = {
    if(pos != 0) {
      ReadableByteChannelUtils.seek(readChannel, pos)
    }
    val refiller = ByteBufferRefiller(readChannel, maxBlockSize)
    refiller.buffer.order(ByteOrder.LITTLE_ENDIAN)
    val reader = ByteBufferReader(refiller)
    new BlockEitherator(reader, pos)
  }

  def readAllBlocks(readChannel: ReadableByteChannel): Either[Snag, Seq[BGZBlock]] = {
    val refiller = ByteBufferRefiller(readChannel, maxBlockSize)
    refiller.buffer.order(ByteOrder.LITTLE_ENDIAN)
    var snagOpt: Option[Snag] = None
    var blocks: Seq[BGZBlock] = Seq.empty
    val reader = ByteBufferReader(refiller)
    var moreBlocksExpected: Boolean = true
    while(moreBlocksExpected && snagOpt.isEmpty) {
      readBlock(reader) match {
        case Left(snag) => snagOpt = Some(snag)
        case Right(block) =>
          if(block.isEOFMarker) {
            moreBlocksExpected = false
          } else {
            blocks :+= block
          }
      }
    }
    snagOpt match {
      case Some(snag) => Left(snag)
      case None => Right(blocks)
    }
  }

  def readBlock(reader: ByteBufferReader): Either[Snag, BGZBlock] = {
    reader.refiller.buffer.mark()
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