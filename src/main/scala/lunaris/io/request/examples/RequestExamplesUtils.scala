package lunaris.io.request.examples

import lunaris.genomics.Region
import lunaris.recipes.tools.ToolCall
import lunaris.recipes.tools.ToolCall.{RefArg, ValueArg}
import lunaris.recipes.tools.native.{IndexedObjectReader, JSONWriter, TSVWriter}
import lunaris.recipes.values.LunValue.MapValue
import lunaris.recipes.values.LunValue.PrimitiveValue.{FileValue, StringValue}

object RequestExamplesUtils {

  object PortalData {
    val folder: String = "gs://fc-6fe31e1f-2c36-411c-bf23-60656d621184/data/t2d/"
    val idField: StringValue = StringValue("varId")
    val associations: FileValue = FileValue(folder + "associations.tsv.gz")
    val associationsDkd: FileValue = FileValue(folder + "associations.dkd.tsv.gz")
    val posteriors: FileValue = FileValue(folder + "posteriors.tsv.gz")
    val regions: FileValue = FileValue(folder + "regions.tsv.gz")
    val variants: FileValue = FileValue(folder + "variants.tsv.gz")
  }

  object Regions {
    val simpleRegion: Map[String, Seq[Region]] = Map("1" -> Seq(Region(100000, 200000)))
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
                            idField: StringValue,
                            typesOpt: Option[MapValue] = None): ToolCall = {
      val args = Utils.buildArgs(
        IndexedObjectReader.Params.Keys.file -> ValueArg(IndexedObjectReader.Params.file, file),
        IndexedObjectReader.Params.Keys.idField -> ValueArg(IndexedObjectReader.Params.idField, idField)
      )(
        typesOpt.map(IndexedObjectReader.Params.Keys.types -> ValueArg(IndexedObjectReader.Params.types, _))
      )
      ToolCall(IndexedObjectReader, args)
    }

    def tsvWriter(from: String, file: FileValue): ToolCall = ToolCall(TSVWriter, Map(
      TSVWriter.Params.Keys.from -> RefArg(TSVWriter.Params.from, from),
      TSVWriter.Params.Keys.file -> ValueArg(TSVWriter.Params.file, file)
    ))

    def jsonWriter(from: String, file: FileValue): ToolCall = ToolCall(JSONWriter, Map(
      JSONWriter.Params.Keys.from -> RefArg(JSONWriter.Params.from, from),
      JSONWriter.Params.Keys.file -> ValueArg(JSONWriter.Params.file, file)
    ))
  }

}
