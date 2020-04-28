package lunaris.io.tbi

import java.nio.ByteOrder

import lunaris.io.ByteBufferReader
import org.broadinstitute.yootilz.core.snag.Snag

case class TBIFileHeader(n_ref: Int, format: Int, col_seq: Int, col_beg: Int, col_end: Int, meta: Int, skip: Int,
                         l_nm: Int, names: Seq[String])

object TBIFileHeader {
  def parseNames(namesBytes: Array[Byte]): Either[Snag, Seq[String]] = {
    val lastByte = namesBytes.last
    if (lastByte != 0.toByte) {
      Left(Snag(s"Last byte of names field needs to be zero, but is $lastByte."))
    } else {
      var snagOpt: Option[Snag] = None
      var lastDividerPos: Int = -1
      var names: Seq[String] = Seq.empty
      var pos: Int = 0
      while (snagOpt.isEmpty && pos < namesBytes.length) {
        if (namesBytes(pos) == 0) {
          val nameStart = lastDividerPos + 1
          if (nameStart == pos) {
            snagOpt = Some(Snag("Names field has two consecutive zeros - empty sequence name?"))
          } else {
            val name = new String(namesBytes.slice(nameStart, pos))
            names :+= name
            lastDividerPos = pos
          }
        }
        pos += 1
      }
      snagOpt match {
        case Some(snag) => Left(snag)
        case None => Right(names)
      }
    }
  }

  def assertNNames(n_ref: Int, nNames: Int): Either[Snag, Unit] = {
    if (n_ref == nNames) {
      Right(())
    } else {
      Left(Snag(s"Number of sequence names should be $n_ref, but is $nNames."))
    }
  }

  def read(reader: ByteBufferReader): Either[Snag, TBIFileHeader] = {
    for {
      _ <- reader.readByteFieldAssert("magic1", 'T'.toByte)
      _ <- reader.readByteFieldAssert("magic2", 'B'.toByte)
      _ <- reader.readByteFieldAssert("magic3", 'I'.toByte)
      _ <- reader.readByteFieldAssert("magic4", 1.toByte)
      n_ref <- reader.readIntField("n_ref")
      format <- reader.readIntField("format")
      col_seq <- reader.readIntField("col_seq")
      col_beg <- reader.readIntField("col_beg")
      col_end <- reader.readIntField("col_end")
      meta <- reader.readIntField("meta")
      skip <- reader.readIntField("skip")
      l_nm <- reader.readIntField("l_nm")
      namesRaw <- reader.readBytes(l_nm)
      names <- parseNames(namesRaw)
      _ <- assertNNames(n_ref, names.size)
    } yield TBIFileHeader(n_ref, format, col_seq, col_beg, col_end, meta, skip, l_nm, names)
  }
}