package lunaris.io.bgz

import java.nio.ByteOrder
import java.nio.channels.ReadableByteChannel

import lunaris.io.{ByteBufferReader, ByteBufferRefiller}
import lunaris.utils.{Eitherator, ReadableByteChannelUtils}
import org.broadinstitute.yootilz.core.snag.Snag

case class BGZBlock(header: BGZHeader, footer: BGZFooter, unzippedData: BGZUnzippedData) {
  def isEOFMarker: Boolean = unzippedData.bytes.length == 0 // per BGZF specs, EOF block with empty payload
}

object BGZBlock {
  val maxBlockSize: Int = 65536 //  Math.pow(2, 16).toInt, per BGZF specs

  def readBlock(readChannel: ReadableByteChannel): Either[Snag, BGZBlock] = {
    val refiller = ByteBufferRefiller(readChannel, maxBlockSize)
    refiller.buffer.order(ByteOrder.LITTLE_ENDIAN)
    val reader = ByteBufferReader(refiller)
    readBlock(reader)
  }

  class BlockEitherator(refiller: ByteBufferRefiller.Seekable) extends Eitherator[BGZBlock] {

    case class BlockWithPos(block: BGZBlock, pos: Long) {
      def nextPos: Long = pos + block.header.bsize.toPositiveLong
    }

    refiller.buffer.order(ByteOrder.LITTLE_ENDIAN)
    val reader: ByteBufferReader = ByteBufferReader(refiller)
    var lastBlockWithPosOpt: Option[BlockWithPos] = None
    var haveReadEOFBlock: Boolean = false
    var readPos: Long = 0

    private def handleBlock(block: BGZBlock): Option[BGZBlock] = {
      lastBlockWithPosOpt = Some(BlockWithPos(block, readPos))
      readPos += block.header.bsize.toPositiveLong
      if (block.isEOFMarker) {
        haveReadEOFBlock = true
        None
      } else {
        Some(block)
      }
    }

    override def next(): Either[Snag, Option[BGZBlock]] = {
      if (haveReadEOFBlock) {
        lastBlockWithPosOpt = None
        Right(None)
      } else {
        lastBlockWithPosOpt match {
          case Some(BlockWithPos(block, pos)) if pos == readPos =>
            println(s"Going to reuse block at $readPos.")
            Right(handleBlock(block))
          case _ =>
            println(s"Going to read block at $readPos.")
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
    refiller.buffer.order(ByteOrder.LITTLE_ENDIAN)
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