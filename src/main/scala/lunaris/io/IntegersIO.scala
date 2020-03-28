package lunaris.io

object IntegersIO {

  def byteToBinaryString(byte: Byte): String = {
    val intBinaryString = byte.toBinaryString
    if (intBinaryString.length <= 8) {
      intBinaryString
    } else {
      intBinaryString.substring(intBinaryString.length - 8)
    }
  }

  class UnsignedByte(val byte: Byte) extends AnyVal {
    def toPositiveByteOpt: Option[Byte] = Option.when(byte >= 0)(byte)

    def toPositiveInt: Int = {
      if (byte >= 0) {
        byte.toInt
      } else {
        byte.toInt + UnsignedByte.shiftToPositive
      }
    }

    def toPositiveLong: Long = {
      if (byte >= 0) {
        byte.toLong
      } else {
        byte.toLong + UnsignedByte.shiftToPositive
      }
    }

    override def toString: String = toPositiveInt.toString
  }

  object UnsignedByte {
    def apply(byte: Byte): UnsignedByte = new UnsignedByte(byte)

    val shiftToPositive: Int = 256
  }

  class UnsignedShort(val short: Short) extends AnyVal {
    def toPositiveShortOpt: Option[Short] = Option.when(short >= 0)(short)

    def toPositiveInt: Int = {
      if (short >= 0) {
        short.toInt
      } else {
        short.toInt + UnsignedShort.shiftToPositive
      }
    }

    def toPositiveLong: Long = {
      if (short >= 0) {
        short.toLong
      } else {
        short.toLong + UnsignedShort.shiftToPositive
      }
    }

    override def toString: String = toPositiveInt.toString
  }

  object UnsignedShort {
    def apply(short: Short): UnsignedShort = new UnsignedShort(short)

    val shiftToPositive: Int = 65536
  }

  class UnsignedInt(val int: Int) extends AnyVal {
    def toPositiveIntOpt: Option[Int] = Option.when(int >= 0)(int)

    def toPositiveLong: Long = {
      if (int >= 0) {
        int.toLong
      } else {
        int.toLong + UnsignedInt.shiftToPositive
      }
    }

    override def toString: String = toPositiveLong.toString
  }

  object UnsignedInt {
    def apply(int: Int): UnsignedInt = new UnsignedInt(int)

    val shiftToPositive: Long = 4294967296L
  }

  class UnsignedLong(val long: Long) extends AnyVal {
    def toPositiveLongOpt: Option[Long] = Option.when(long >= 0)(long)
  }

  object UnsignedLong {
    def apply(long: Long): UnsignedLong = new UnsignedLong(long)
  }

}
