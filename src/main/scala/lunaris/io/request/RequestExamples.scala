package lunaris.io.request

import lunaris.genomics.Region
import lunaris.io.OutputId
import lunaris.recipes.Recipe
import lunaris.recipes.tools.ToolCall
import lunaris.recipes.tools.ToolCall.{RefArg, ValueArg}
import lunaris.recipes.tools.native.{IndexedDataReader, TSVWriter}
import lunaris.recipes.values.LunValue.PrimitiveValue.{FileValue, StringValue}
import lunaris.utils.IOUtils

object RequestExamples {

  val example1: Request =
    Request("example1",
      Map("1" -> Seq(Region(100000, 200000))),
      Recipe(Map(
        "read" -> ToolCall(IndexedDataReader, Map(
          IndexedDataReader.Params.Keys.file ->
            ValueArg(IndexedDataReader.Params.file,
              FileValue("gs://fc-6fe31e1f-2c36-411c-bf23-60656d621184/data/t2d/associations.tsv.gz")),
          IndexedDataReader.Params.Keys.idField -> ValueArg(IndexedDataReader.Params.idField, StringValue("varId"))
        )
        ),
        "write" -> ToolCall(TSVWriter, Map(
          "from" -> RefArg(TSVWriter.Params.from, "read")
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
