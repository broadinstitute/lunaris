package lunaris.io

import java.io.RandomAccessFile
import java.nio.channels.{Channels, FileChannel, ReadableByteChannel, WritableByteChannel}

import akka.stream.IOResult
import akka.stream.scaladsl.{FileIO, Source, StreamConverters}
import akka.util.ByteString
import better.files.File
import com.google.auth.oauth2.{GoogleCredentials, ServiceAccountCredentials}
import com.google.cloud.storage.BlobId
import com.google.cloud.{ReadChannel, WriteChannel}
import lunaris.io.Disposable.Disposer
import org.broadinstitute.yootilz.gcp.storage.GoogleStorageUtils

import scala.concurrent.Future
import scala.util.Try

trait IoId {
  def asString: String

  override def toString: String = asString

  def +(suffix: String): IoId

  def /(suffix: String): IoId
}

trait InputId extends IoId {
  def newReadChannelDisposable(resourceConfig: ResourceConfig = ResourceConfig.empty): Disposable[ReadableByteChannel]

  def newReadChannelOffsetDisposable(pos: Long,
                                     resourceConfig: ResourceConfig = ResourceConfig.empty):
  Disposable[ReadableByteChannel]

  def newStream(resourceConfig: ResourceConfig): Source[ByteString, Future[IOResult]]

  def +(suffix: String): InputId

  def /(suffix: String): InputId
}

object InputId {
  def apply(string: String): InputId = {
    GcpBlobId.parseBlobId(string).fold[InputId](FileInputId(File(string)))(GcpBlobInputId)
  }
}

trait OutputId extends IoId {
  def newWriteChannelDisposable(resourceConfig: ResourceConfig = ResourceConfig.empty):
  Disposable[WritableByteChannel]

  def +(suffix: String): OutputId

  def /(suffix: String): OutputId
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
    if (pos != 0) {
      raf.seek(pos)
    }
    Disposable.forCloseable(raf.getChannel)
  }

  def +(suffix: String): FileIoId
}

case class FileInputId(file: File) extends InputId with FileIoId {
  override def newReadChannelDisposable(resourceConfig: ResourceConfig): Disposable[ReadableByteChannel] =
    newFileChannelDisposable("r", 0)

  override def newReadChannelOffsetDisposable(pos: Long,
                                              resourceConfig: ResourceConfig): Disposable[ReadableByteChannel] = {
    newFileChannelDisposable("r", pos)
  }

  override def newStream(resourceConfig: ResourceConfig): Source[ByteString, Future[IOResult]] = {
    FileIO.fromPath(file.path)
  }

  override def +(suffix: String): FileInputId = FileInputId(File(file.toString + suffix))

  override def /(suffix: String): FileInputId = FileInputId(File(file.toString + "/" + suffix))
}

case class FileOutputId(file: File) extends OutputId with FileIoId {
  override def asString: String = file.toString()

  override def newWriteChannelDisposable(resourceConfig: ResourceConfig): Disposable[WritableByteChannel] = {
    file.clear()
    newFileChannelDisposable("rw", 0)
  }

  override def +(suffix: String): FileOutputId = FileOutputId(File(file.toString + suffix))

  override def /(suffix: String): FileOutputId = FileOutputId(File(file.toString + "/" + suffix))
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

  def +(suffix: String): GcpBlobId
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

  def addSuffix(blobId: BlobId, suffix: String): BlobId = BlobId.of(blobId.getBucket, blobId.getName + suffix)
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

  override def newStream(resourceConfig: ResourceConfig): Source[ByteString, Future[IOResult]] = {
    StreamConverters.fromInputStream(() => Channels.newInputStream(newReadChannel(resourceConfig)))
  }

  override def +(suffix: String): GcpBlobInputId = GcpBlobInputId(GcpBlobId.addSuffix(blobId, suffix))

  override def /(suffix: String): GcpBlobInputId = GcpBlobInputId(GcpBlobId.addSuffix(blobId, "/" + suffix))
}

case class GcpBlobOutputId(blobId: BlobId) extends GcpBlobId with OutputId {
  private def newWriteChannel(resourceConfig: ResourceConfig): WriteChannel = {
    storageUtils(resourceConfig).writerToNewBlob(blobId)
  }

  override def newWriteChannelDisposable(resourceConfig: ResourceConfig): Disposable[WritableByteChannel] = {
    val writeChannel = newWriteChannel(resourceConfig)
    Disposable(writeChannel)(Disposer.ForCloseable(writeChannel))
  }

  override def +(suffix: String): GcpBlobOutputId = GcpBlobOutputId(GcpBlobId.addSuffix(blobId, suffix))

  override def /(suffix: String): GcpBlobOutputId = GcpBlobOutputId(GcpBlobId.addSuffix(blobId, "/" + suffix))
}

