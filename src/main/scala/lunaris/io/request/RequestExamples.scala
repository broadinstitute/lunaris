package lunaris.io.request

import lunaris.genomics.Region
import lunaris.io.OutputId
import lunaris.recipes.Recipe
import lunaris.recipes.tools.ToolCall
import lunaris.recipes.tools.ToolCall.{RefArg, ValueArg}
import lunaris.recipes.tools.native.{IndexedObjectReader, IndexedRecordReader, JSONWriter, TSVWriter}
import lunaris.recipes.values.LunValue.PrimitiveValue.{FileValue, StringValue}
import lunaris.utils.IOUtils

object RequestExamples {

  object PortalData {
    val folder: String = "gs://fc-6fe31e1f-2c36-411c-bf23-60656d621184/data/t2d/"

    val associations: FileValue = FileValue(folder + "associations.tsv.gz")
    val associationsDkd: FileValue = FileValue(folder + "associations.dkd.tsv.gz")
    val posteriors: FileValue = FileValue(folder + "posteriors.tsv.gz")
    val regions: FileValue = FileValue(folder + "regions.tsv.gz")
    val variants: FileValue = FileValue(folder + "variants.tsv.gz")

  }

  val example1: Request =
    Request("example1",
      Map("1" -> Seq(Region(100000, 200000))),
      Recipe(Map(
        "read" -> ToolCall(IndexedObjectReader, Map(
          IndexedObjectReader.Params.Keys.file ->
            ValueArg(IndexedObjectReader.Params.file, PortalData.variants),
          IndexedObjectReader.Params.Keys.idField -> ValueArg(IndexedObjectReader.Params.idField, StringValue("varId"))
        )
        ),
        "write" -> ToolCall(JSONWriter, Map(
          "from" -> RefArg(JSONWriter.Params.from, "read")
        ))
      ))
    )

  def main(args: Array[String]): Unit = {
    val fileName =
      if (args.length > 0) {
        args(0)
      } else {
        "example1.request.lunaris"
      }
    val outputId = OutputId(fileName)
    val example1String = RequestJson.serialize(example1)
    println(example1String)
    outputId.newWriteChannelDisposable().useUp { writeChannel =>
      IOUtils.writeStringToChannel(example1String, writeChannel)
    }
  }
}
