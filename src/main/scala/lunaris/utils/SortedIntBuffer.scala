package lunaris.utils

final class SortedIntBuffer(initialBufferSize: Int = SortedIntBuffer.bufferMinSize) {
  private var nInts : Int = 0
  private var array: Array[Int] = new Array[Int](initialBufferSize)

  private def bufferSize: Int = array.length

  private def growBuffer(): Unit = {
    val bufferSizeNew = array.length + (array.length / 7) + SortedIntBuffer.bufferMinSize
    val arrayNew = new Array[Int](bufferSizeNew)
    System.arraycopy(array, 0, arrayNew, 0, nInts)
    array = arrayNew
  }

  private def cropBuffer(): Unit = {
    val arrayNew = new Array[Int](nInts)
    System.arraycopy(array, 0, arrayNew, 0, nInts)
  }

  def insert(pos: Int, ints: Seq[Int]): Unit = {
    val nInsert = ints.size
    while(nInts + nInsert > bufferSize) {
      growBuffer()
    }
    System.arraycopy(array, pos, array, pos+nInsert, nInts - pos)
    var iIntToInsert = pos
    ints.foreach { intToInsert =>
      array(iIntToInsert) = intToInsert
      iIntToInsert += 1
    }
  }

  def toSeq(): Seq[Int] = {
    cropBuffer()
    array.toIndexedSeq
  }

  def size: Int = nInts
}

object SortedIntBuffer {
  val bufferMinSize: Int = 10
}
