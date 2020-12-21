package lunaris.io.request.examples

import lunaris.genomics.Region
import lunaris.recipes.tools.ToolCall
import lunaris.recipes.tools.ToolCall.{RefArg, ValueArg}
import lunaris.recipes.tools.builtin.{CalculateMaf, GroupFileWriter, IdCanonicalizer, IndexedRecordReader, JSONWriter, JoinRecordsWithFallback, RecordsFilter, RecordsSimpleFilter, TSVWriter, VcfRecordsReader}
import lunaris.recipes.values.LunValue.{ArrayValue, ExpressionValue, MapValue}
import lunaris.recipes.values.LunValue.PrimitiveValue.{FileValue, StringValue}

object RequestBuildUtils {

  object PortalData {
    val folder: String = "gs://fc-6fe31e1f-2c36-411c-bf23-60656d621184/data/t2d/"

    object Fields {
      val varId: StringValue = StringValue("var_id")
      val phenotype: StringValue = StringValue("phenotype")
    }

    object Files {
      val associations: FileValue = FileValue(folder + "associations.tsv.gz")
      val associationsDkd: FileValue = FileValue(folder + "associations.dkd.tsv.gz")
      val posteriors: FileValue = FileValue(folder + "posteriors.tsv.gz")
      val regions: FileValue = FileValue(folder + "regions.tsv.gz")
      val variants: FileValue = FileValue(folder + "variants.tsv.gz")
    }

  }

  object Regions {
    val simpleRegion: Map[String, Seq[Region]] = Map("1" -> Seq(Region(100000, 200000)))
    val somewhatBiggerRegion: Map[String, Seq[Region]] = Map("1" -> Seq(Region(0, 1000000)))
    val moreRegions: Map[String, Seq[Region]] =
      Map(
        "1" -> Seq(Region(100000, 200000), Region(300000, 400000)),
        "X" -> Seq(Region(0, 100000), Region(400000, 500000))
      )
  }

  object ToolCalls {

    object Utils {
      def buildArgs(args: (String, ToolCall.Arg)*)(argOpts: Option[(String, ToolCall.Arg)]*):
      Map[String, ToolCall.Arg] = {
        args.toMap ++ argOpts.flatten.toMap
      }
    }

    def indexedObjectReader(file: FileValue,
                            indexOpt: Option[FileValue],
                            idField: StringValue,
                            typesOpt: Option[MapValue] = None): ToolCall = {
      val args = Utils.buildArgs(
        IndexedRecordReader.Params.Keys.file -> ValueArg(IndexedRecordReader.Params.file, file),
        IndexedRecordReader.Params.Keys.idField -> ValueArg(IndexedRecordReader.Params.idField, idField)
      )(
        indexOpt.map(IndexedRecordReader.Params.Keys.index -> ValueArg(IndexedRecordReader.Params.index, _)),
        typesOpt.map(IndexedRecordReader.Params.Keys.types -> ValueArg(IndexedRecordReader.Params.types, _)),
      )
      ToolCall(IndexedRecordReader, args)
    }

    def vcfRecordsReader(file: FileValue, chroms: ArrayValue): ToolCall = {
      ToolCall(VcfRecordsReader, Map(
        VcfRecordsReader.Params.Keys.file -> ValueArg(VcfRecordsReader.Params.file, file),
        VcfRecordsReader.Params.Keys.chroms -> ValueArg(VcfRecordsReader.Params.chroms, chroms)
      ))
    }

    def joinRecordsWithFallback(driver: String, data: String, fallback: StringValue): ToolCall = {
      ToolCall(JoinRecordsWithFallback, Map(
        JoinRecordsWithFallback.Params.Keys.driver -> RefArg(JoinRecordsWithFallback.Params.driver, driver),
        JoinRecordsWithFallback.Params.Keys.data -> RefArg(JoinRecordsWithFallback.Params.data, data),
        JoinRecordsWithFallback.Params.Keys.fallback -> ValueArg(JoinRecordsWithFallback.Params.fallback, fallback),
      ))
    }

    def filter(from: String, filter: ExpressionValue): ToolCall = {
      ToolCall(RecordsFilter, Map(
        RecordsFilter.Params.Keys.from -> RefArg(RecordsFilter.Params.from, from),
        RecordsFilter.Params.Keys.filter -> ValueArg(RecordsFilter.Params.filter, filter)
      ))
    }

    def simpleFilter(from: String, field: StringValue, stringValue: StringValue): ToolCall =
      ToolCall(RecordsSimpleFilter, Map(
        RecordsSimpleFilter.Params.Keys.from -> RefArg(RecordsSimpleFilter.Params.from, from),
        RecordsSimpleFilter.Params.Keys.field -> ValueArg(RecordsSimpleFilter.Params.field, field),
        RecordsSimpleFilter.Params.Keys.stringValue -> ValueArg(RecordsSimpleFilter.Params.stringValue, stringValue)
      ))

    def tsvWriter(from: String, file: FileValue): ToolCall = ToolCall(TSVWriter, Map(
      TSVWriter.Params.Keys.from -> RefArg(TSVWriter.Params.from, from),
      TSVWriter.Params.Keys.file -> ValueArg(TSVWriter.Params.file, file)
    ))

    def jsonWriter(from: String, file: FileValue): ToolCall = ToolCall(JSONWriter, Map(
      JSONWriter.Params.Keys.from -> RefArg(JSONWriter.Params.from, from),
      JSONWriter.Params.Keys.file -> ValueArg(JSONWriter.Params.file, file)
    ))

    def idCanonicalizer(from: String, refField: StringValue, altField: StringValue,
                        idFieldNew: StringValue): ToolCall =
      ToolCall(IdCanonicalizer, Map(
        IdCanonicalizer.Params.Keys.from -> RefArg(IdCanonicalizer.Params.from, from),
        IdCanonicalizer.Params.Keys.refField -> ValueArg(IdCanonicalizer.Params.refField, refField),
        IdCanonicalizer.Params.Keys.altField -> ValueArg(IdCanonicalizer.Params.altField, altField),
        IdCanonicalizer.Params.Keys.idFieldNew -> ValueArg(IdCanonicalizer.Params.idFieldNew, idFieldNew)
      ))

    def groupFileWriter(from: String, file: FileValue, format: StringValue): ToolCall =
      ToolCall(GroupFileWriter, Map(
        GroupFileWriter.Params.Keys.from -> RefArg(GroupFileWriter.Params.from, from),
        GroupFileWriter.Params.Keys.file -> ValueArg(GroupFileWriter.Params.file, file),
        GroupFileWriter.Params.Keys.format -> ValueArg(GroupFileWriter.Params.format, format)
      ))

    def calculateMaf(from: String): ToolCall =
      ToolCall(CalculateMaf, Map(
        CalculateMaf.Params.Keys.from -> RefArg(CalculateMaf.Params.from, from)
      ))
  }

}
