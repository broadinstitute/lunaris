package lunaris.data

import java.nio.file.{Path, Paths}

case class DataSourceWithIndex(dataSource: Path, index: Path)

object DataSourceWithIndex {
  def apply(dataSource: String, index: String): DataSourceWithIndex =
    DataSourceWithIndex(Paths.get(dataSource), Paths.get(index))
}