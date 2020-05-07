package lunaris.utils

import java.nio.ByteBuffer
import java.nio.channels.{ReadableByteChannel, WritableByteChannel}
import java.nio.charset.StandardCharsets

import lunaris.io.{ByteBufferReader, ByteBufferRefiller}
import org.broadinstitute.yootilz.core.snag.{Snag, SnagTag}

object IOUtils {

  def writeStringToChannel(string: String, channel: WritableByteChannel): Unit = {
    val buffer = ByteBuffer.wrap(string.getBytes(StandardCharsets.UTF_8))
    channel.write(buffer)
  }

  def readStringFromChannel(channel: ReadableByteChannel, bufferSize: Int = 65536): Either[Snag, String] = {
    val refiller = ByteBufferRefiller(channel, bufferSize)
    val reader = ByteBufferReader(refiller)
    var snagOpt: Option[Snag] = None
    var channelIsExhausted: Boolean = false
    val builder = Array.newBuilder[Byte]
    while(snagOpt.isEmpty && !channelIsExhausted) {
      reader.readByteField("byte") match {
        case Left(snag) =>
          if(snag.tags.contains(SnagTag.endOfData)) {
            channelIsExhausted = true
          } else {
            snagOpt = Some(snag)
          }
        case Right(byte) => builder += byte
      }
    }
    snagOpt match {
      case Some(snag) => Left(snag)
      case None => Right(new String(builder.result))
    }
  }

}
