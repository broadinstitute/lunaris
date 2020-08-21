package lunaris.streams

import lunaris.streams.utils.RecordStreamTypes.{Meta, Record, RecordSource}
import org.broadinstitute.yootilz.core.snag.Snag

object RecordStreamZipperWithFallback {

  def zipWithFallback(meta: Meta,
                      driverStream: RecordSource,
                      dataStream: RecordSource)(
                       fallBack: Record => Either[Snag, Record]
                     ): RecordSource = {
    
    ???
  }

}
