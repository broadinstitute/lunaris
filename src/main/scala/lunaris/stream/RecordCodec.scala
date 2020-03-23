package lunaris.stream

import htsjdk.tribble.AsciiFeatureCodec
import htsjdk.tribble.readers.LineIterator

object RecordCodec extends AsciiFeatureCodec[Record](classOf[Record]){
  override def decode(s: String): Record = ???

  override def readActualHeader(reader: LineIterator): AnyRef = ???

  override def canDecode(path: String): Boolean = ???
}
