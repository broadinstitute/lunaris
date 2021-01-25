package musha

import musha.sql.SqlColumn.Flag
import musha.sql.{SqlColumn, SqlColumnValue}

import java.sql.ResultSet

trait SqlCodec[A, B] {
  def sqlColumn: SqlColumn[A]

  def withValue(value: B): SqlColumnValue[A]

  def get: ResultSet => B

  def asPrimaryKey: SqlCodec[A, B]

  def bimap[C](encode: C => B, decode: B => C): SqlCodec.Mapped[A, C]
}

object SqlCodec {

  class Mapped[A, B](val sqlColumn: SqlColumn[A], encode: B => A, decode: A => B) extends SqlCodec[A, B] {
    override def withValue(value: B): SqlColumnValue[A] = SqlColumnValue[A](sqlColumn, encode(value))

    override def get: ResultSet => B = sqlColumn.get.andThen(decode)

    override def bimap[C](encode: C => B, decode: B => C): Mapped[A, C] = {
      val encodeComposed = encode.andThen(this.encode)
      val decodeComposed = this.decode.andThen(decode)
      Mapped[A, C](sqlColumn, encodeComposed, decodeComposed)
    }

    override def asPrimaryKey: SqlCodec[A, B] = Mapped(sqlColumn.asPrimaryKey, encode, decode)
  }

  object Mapped {
    def apply[A, B](sqlColumn: SqlColumn[A], encode: B => A, decode: A => B): Mapped[A, B] =
      new Mapped(sqlColumn, encode, decode)
  }

}