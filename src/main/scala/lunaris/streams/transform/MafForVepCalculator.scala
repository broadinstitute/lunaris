package lunaris.streams.transform

import lunaris.recipes.values.{LunType, LunValue}
import lunaris.streams.utils.RecordStreamTypes.Record

object MafForVepCalculator {

  def addMaf(record: Record): Record = {
    val mafKey = "MAF"
    record.values.get(mafKey) match {
      case Some(_) => record
      case None =>
        var maf: Double = 0.0
        val keyLengthMin: Int = "gnomAD_exomes_*_AF".length
        for ((key, value) <- record.values) {
          if (key.length >= keyLengthMin && key.startsWith("gnomAD_exomes_") && key.endsWith("_AF")) {
            value.castTo(LunType.FloatType).flatMap(_.asDouble) match {
              case Left(_) => ()
              case Right(mafCohort) => maf = Math.max(maf, Math.min(mafCohort, 1.0 - mafCohort))
            }
          }
        }
        record.addField("MAF", LunValue.PrimitiveValue.FloatValue(maf), LunType.FloatType) match {
          case Left(_) => record
          case Right(recordNew) => recordNew
        }
    }
  }

}
