package lunaris.io.query

import lunaris.data.DataSourceWithIndex
import lunaris.genomics.Regions
import lunaris.stream.Record
import lunaris.utils.Eitherator

object RecordExtractor {

  def extract(dataSourceWithIndex: DataSourceWithIndex, regions: Regions): Eitherator[Record] = {
    ???
  }

}
