package lunaris.data

import java.nio.file.Path

import htsjdk.samtools.util.IOUtil

case class DataSourceWithIndex(dataSource: Path, index: Path)

object DataSourceWithIndex {
  def apply(dataSource: String, index: String): DataSourceWithIndex =
    DataSourceWithIndex(IOUtil.getPath(dataSource), IOUtil.getPath(index))
}