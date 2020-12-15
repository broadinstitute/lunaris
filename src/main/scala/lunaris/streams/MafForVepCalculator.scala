package lunaris.streams

import lunaris.streams.utils.RecordStreamTypes.Record

object MafForVepCalculator {

  def addMaf(record: Record): Record = {
    val mafKey = "MAF"
    record.values.get(mafKey) match {
      case Some(_) => record
      case None =>

        ??? // gnomAD_exomes_AFR_AF
    }
  }

}
