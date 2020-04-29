package lunaris.utils

import java.nio.channels.{ReadableByteChannel, SeekableByteChannel}

import com.google.cloud.ReadChannel
import org.broadinstitute.yootilz.core.snag.Snag

object ReadableByteChannelUtils {

  def seek(channel: ReadableByteChannel, pos: Long): Either[Snag, Unit] = {
    channel match {
      case seekableChannel: SeekableByteChannel =>
        seekableChannel.position(pos)
        Right(())
      case gcpReadChannel: ReadChannel =>
        gcpReadChannel.seek(pos)
        Right(())
      case _ =>
        Left(Snag(s"Don't know how to set position on a ${channel.getClass.getCanonicalName}."))
    }
  }
}
