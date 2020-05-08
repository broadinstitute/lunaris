package lunaris.data

import lunaris.io.InputId

case class BlockGzippedWithIndex(data: InputId, index: InputId)

object BlockGzippedWithIndex {
  def apply(data: String, index: String): BlockGzippedWithIndex =
    BlockGzippedWithIndex(InputId(data), InputId(index))

  def apply(data: InputId): BlockGzippedWithIndex = BlockGzippedWithIndex(data, data + ".tbi")

  def apply(data: String): BlockGzippedWithIndex = BlockGzippedWithIndex(InputId(data))
}