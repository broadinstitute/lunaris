package lunaris.io

import java.io.RandomAccessFile
import java.nio.channels.{Channels, FileChannel, ReadableByteChannel, WritableByteChannel}

import better.files.File
import com.google.auth.oauth2.{GoogleCredentials, ServiceAccountCredentials}
import com.google.cloud.storage.BlobId
import com.google.cloud.{ReadChannel, WriteChannel}
import lunaris.io.Disposable.Disposer
import org.broadinstitute.yootilz.gcp.storage.GoogleStorageUtils

import scala.util.Try

trait IoId {
  def asString: String

  override def toString: String = asString
}

trait InputId extends IoId {
  def newReadChannelDisposable(resourceConfig: ResourceConfig = ResourceConfig.empty): Disposable[ReadableByteChannel]

  def newReadChannelOffsetDisposable(pos: Long,
                                     resourceConfig: ResourceConfig = ResourceConfig.empty):
  Disposable[ReadableByteChannel]
}

object InputId {
  def apply(string: String): InputId = {
    GcpBlobId.parseBlobId(string).fold[InputId](FileInputId(File(string)))(GcpBlobInputId)
  }
}

trait OutputId extends IoId {
  def newWriteChannelDisposable(resourceConfig: ResourceConfig = ResourceConfig.empty):
  Disposable[WritableByteChannel]
}

object OutputId {
  def apply(string: String): OutputId = {
    GcpBlobId.parseBlobId(string).fold[OutputId](FileOutputId(File(string)))(GcpBlobOutputId)
  }
}

trait FileIoId extends IoId {
  def file: File

  override def asString: String = file.toString()

  def newFileChannelDisposable(mode: String, pos: Long): Disposable[FileChannel] = {
    val raf = new RandomAccessFile(file.toJava, mode)
    if(pos != 0) {
      raf.seek(pos)
    }
    Disposable(raf.getChannel)(Disposer.ForCloseable(raf))
  }
}

case class FileInputId(file: File) extends InputId with FileIoId {
  override def newReadChannelDisposable(resourceConfig: ResourceConfig): Disposable[ReadableByteChannel] =
    newFileChannelDisposable("r", 0)

  override def newReadChannelOffsetDisposable(pos: Long,
                                              resourceConfig: ResourceConfig): Disposable[ReadableByteChannel] = {
    newFileChannelDisposable("r", pos)
  }
}

case class FileOutputId(file: File) extends OutputId with FileIoId {
  override def asString: String = file.toString()

  override def newWriteChannelDisposable(resourceConfig: ResourceConfig): Disposable[WritableByteChannel] =
    newFileChannelDisposable("rw", 0)
}

trait GcpBlobId extends IoId {
  def blobId: BlobId

  override def asString: String = blobId.toString

  protected def storageUtils(resourceConfig: ResourceConfig): GoogleStorageUtils = {
    val keyFileInputStreamOpt = resourceConfig.keyFileOpt.map(_.newReadChannelDisposable(ResourceConfig.empty))
    val creds = keyFileInputStreamOpt.flatMap { serviceAccountInDisposable =>
      Try {
        serviceAccountInDisposable.useUp { serviceAccountReadChannel =>
          ServiceAccountCredentials.fromStream(Channels.newInputStream(serviceAccountReadChannel))
        }
      }.toOption
    }.getOrElse(GoogleCredentials.getApplicationDefault).createScoped()
    //    val credentials = OAuthUtils.getCredentials(keyFileInputStreamOpt)
    GoogleStorageUtils(creds, resourceConfig.gcpProjectOpt)
  }
}

object GcpBlobId {
  def parseBlobId(string: String): Option[BlobId] = {
    if (string.startsWith("gs://")) {
      val stringMinusPrefix = string.substring(5)
      val slashPos = stringMinusPrefix.indexOf('/')
      if (slashPos > 0 && slashPos < stringMinusPrefix.length - 1) {
        val bucketName = stringMinusPrefix.substring(0, slashPos)
        val objectName = stringMinusPrefix.substring(slashPos + 1)
        Some(BlobId.of(bucketName, objectName))
      } else {
        None
      }
    } else {
      None
    }
  }
}

case class GcpBlobInputId(blobId: BlobId) extends GcpBlobId with InputId {
  private def newReadChannel(resourceConfig: ResourceConfig): ReadChannel = storageUtils(resourceConfig).reader(blobId)

  override def newReadChannelDisposable(resourceConfig: ResourceConfig): Disposable[ReadableByteChannel] = {
    val readChannel = newReadChannel(resourceConfig)
    Disposable(readChannel)(Disposer.ForCloseable(readChannel))
  }

  override def newReadChannelOffsetDisposable(pos: Long,
                                              resourceConfig: ResourceConfig): Disposable[ReadableByteChannel] = {
    val readChannel = newReadChannel(resourceConfig)
    readChannel.seek(pos)
    Disposable(readChannel)(Disposer.ForCloseable(readChannel))
  }
}

case class GcpBlobOutputId(blobId: BlobId) extends GcpBlobId with OutputId {
  private def newWriteChannel(resourceConfig: ResourceConfig): WriteChannel = {
    storageUtils(resourceConfig).writerToNewBlob(blobId)
  }

  override def newWriteChannelDisposable(resourceConfig: ResourceConfig): Disposable[WritableByteChannel] = {
    val writeChannel = newWriteChannel(resourceConfig)
    Disposable(writeChannel)(Disposer.ForCloseable(writeChannel))
  }
}

