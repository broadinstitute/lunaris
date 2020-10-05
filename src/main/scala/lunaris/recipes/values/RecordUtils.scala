package lunaris.recipes.values

import lunaris.streams.utils.RecordStreamTypes.Record
import org.broadinstitute.yootilz.core.snag.Snag

object RecordUtils {

  def getString(record: Record, fieldName: String): Either[Snag, String] = {
    record.values.get(fieldName) match {
      case None => Left(Snag(s"Record has no value for field $fieldName"))
      case Some(value) => value.asString
    }
  }

}
