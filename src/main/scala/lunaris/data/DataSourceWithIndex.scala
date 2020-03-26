package lunaris.data

import lunaris.io.InputId

case class DataSourceWithIndex(dataSource: InputId, index: InputId)

object DataSourceWithIndex {
  def apply(dataSource: String, index: String): DataSourceWithIndex =
    DataSourceWithIndex(InputId(dataSource), InputId(index))
}