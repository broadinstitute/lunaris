package lunaris.io.bgz

import java.nio.ByteOrder
import java.nio.channels.ReadableByteChannel

import lunaris.io.{ByteBufferReader, ByteBufferRefiller}
import lunaris.utils.{DebugUtils, Eitherator, ReadableByteChannelUtils}
import org.broadinstitute.yootilz.core.snag.Snag

case class BGZBlock(header: BGZHeader, footer: BGZFooter, unzippedData: BGZUnzippedData) {
  def isEOFMarker: Boolean = unzippedData.bytes.length == 0 // per BGZF specs, EOF block with empty payload
}

object BGZBlock {
  val maxBlockSize: Int = 65536 //  Math.pow(2, 16).toInt, per BGZF specs

  case class BGZBlockWithPos(block: BGZBlock, pos: Long) {
    def nextPos: Long = pos + block.header.blockSize
  }

  def readBlock(readChannel: ReadableByteChannel): Either[Snag, BGZBlock] = {
    val refiller = ByteBufferRefiller(readChannel, maxBlockSize)
    val reader = ByteBufferReader(refiller)
    readBlock(reader)
  }

  class BlockEitherator(refiller: ByteBufferRefiller.Seekable) extends Eitherator[BGZBlockWithPos] {
    val reader: ByteBufferReader = ByteBufferReader(refiller)
    var lastBlockWithPosOpt: Option[BGZBlockWithPos] = None
    var haveReadEOFBlock: Boolean = false
    var readPos: Long = 0

    private def handleBlock(block: BGZBlock): Option[BGZBlockWithPos] = {
      lastBlockWithPosOpt = Some(BGZBlockWithPos(block, readPos))
      readPos += block.header.blockSize
      if (block.isEOFMarker) {
        haveReadEOFBlock = true
        None
      } else {
        lastBlockWithPosOpt
      }
    }

    override def next(): Either[Snag, Option[BGZBlockWithPos]] = {
      if (haveReadEOFBlock) {
        lastBlockWithPosOpt = None
        Right(None)
      } else {
        lastBlockWithPosOpt match {
          case Some(BGZBlockWithPos(block, pos)) if pos == readPos =>
            Right(handleBlock(block))
          case _ =>
            readBlock(reader) match {
              case Left(snag) => Left(Snag(s"Could not read next block at $readPos", snag))
              case Right(block) =>
                Right(handleBlock(block))
            }
        }
      }
    }

    def skipToOffset(offsetOfBlock: Long): Unit = {
      refiller.skipTo(offsetOfBlock)
      readPos = offsetOfBlock
      haveReadEOFBlock = false
    }
  }

  def newBlockEitherator(readChannel: ReadableByteChannel): BlockEitherator = {
    val refiller = ByteBufferRefiller(readChannel, maxBlockSize)
    new BlockEitherator(refiller)
  }

  def readAllBlocks(readChannel: ReadableByteChannel): Either[Snag, Seq[BGZBlock]] = {
    val refiller = ByteBufferRefiller(readChannel, maxBlockSize)
    var snagOpt: Option[Snag] = None
    var blocks: Seq[BGZBlock] = Seq.empty
    val reader = ByteBufferReader(refiller)
    var moreBlocksExpected: Boolean = true
    while (moreBlocksExpected && snagOpt.isEmpty) {
      readBlock(reader) match {
        case Left(snag) => snagOpt = Some(snag)
        case Right(block) =>
          if (block.isEOFMarker) {
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
    for {
      header <- {
        reader.refiller.makeAvailable(18)
        reader.refiller.byteBox.readAndReset {
          BGZHeader.read(reader)
        }
      }
      footer <- {
        reader.refiller.makeAvailable(header.blockSize)
        reader.refiller.byteBox.readAndReset {
          reader.refiller.byteBox.buffer
            .position(reader.refiller.byteBox.buffer.position() + header.blockSize - BGZFooter.nFooterBytes)
          BGZFooter.read(reader)
        }
      }
      unzippedData <- {
        BGZUnzippedData.read(reader, header.blockSize)
      }
    } yield BGZBlock(header, footer, unzippedData)
  }
}