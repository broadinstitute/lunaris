package lunaris.vep.vcf

import lunaris.genomics.{Locus, Variant}
import lunaris.recipes.values.{LunType, LunValue}
import lunaris.streams.utils.RecordStreamTypes.Record

object VcfCore {

  object ICols {
    val chrom: Int = 0
    val pos: Int = 1
    val id: Int = 2
    val ref: Int = 3
    val alt: Int = 4
    val qual: Int = 5
    val filter: Int = 6
    val info: Int = 7
    val format = 8
  }

  object ColNames {
    val chrom = "CHROM"
    val pos = "POS"
    val id = "ID"
    val ref = "REF"
    val alt = "ALT"
    val qual = "QUAL"
    val filter = "FILTER"
    val info = "INFO"
    val format = "FORMAT"
    val seq = Seq(chrom, pos, id, ref, alt, qual, filter, info, format)
    val headerLine: String = seq.mkString("\t")
  }

  val vcfRecordType: LunType.RecordType =
    LunType.RecordType(VcfCore.ColNames.id, VcfCore.ColNames.chrom, VcfCore.ColNames.pos, VcfCore.ColNames.pos)
      .addField(VcfCore.ColNames.ref, LunType.StringType)
      .addField(VcfCore.ColNames.alt, LunType.StringType)
      .addField(VcfCore.ColNames.qual, LunType.StringType)
      .addField(VcfCore.ColNames.filter, LunType.StringType)
      .addField(VcfCore.ColNames.info, LunType.StringType)
      .addField(VcfCore.ColNames.format, LunType.StringType)


  case class VcfCoreRecord(chrom: String, pos: Int, id: String, ref: String, alt: String, qual: String,
                           filter: String, info: String, format: String) {
    def toVariant: Variant = Variant(chrom, pos, ref, alt)

    def toLocus: Locus = toVariant.toLocus

    def toRecord: Record = {
      val locus = toLocus
      val values = Map(
        VcfCore.ColNames.chrom -> LunValue.PrimitiveValue.StringValue(chrom),
        VcfCore.ColNames.pos -> LunValue.PrimitiveValue.IntValue(pos),
        VcfCore.ColNames.id -> LunValue.PrimitiveValue.StringValue(id),
        VcfCore.ColNames.ref -> LunValue.PrimitiveValue.StringValue(ref),
        VcfCore.ColNames.alt -> LunValue.PrimitiveValue.StringValue(alt),
        VcfCore.ColNames.qual -> LunValue.PrimitiveValue.StringValue(qual),
        VcfCore.ColNames.filter -> LunValue.PrimitiveValue.StringValue(filter),
        VcfCore.ColNames.info -> LunValue.PrimitiveValue.StringValue(info),
        VcfCore.ColNames.format -> LunValue.PrimitiveValue.StringValue(format),
      )
      LunValue.RecordValue(id, locus, vcfRecordType, values)
    }

    def toLine: String =
      Seq(chrom, pos.toString, id, ref, alt, qual, filter, info, format).mkString("\t")
  }

}
