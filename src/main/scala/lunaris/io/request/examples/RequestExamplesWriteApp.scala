package lunaris.io.request.examples

import lunaris.io.OutputId
import lunaris.io.request.RequestJson
import lunaris.utils.IOUtils

object RequestExamplesWriteApp {

  val defaultExamplesFolder: OutputId = OutputId("examples/requests")
  val examples: Seq[RequestExample] = Seq(RequestExampleMinimal)

  def main(args: Array[String]): Unit = {
    val outputFolder =
      if (args.length > 0) {
        OutputId(args(0))
      } else {
        defaultExamplesFolder
      }
    for(example <- examples) {
      val outputId = outputFolder / example.id + ".json"
      println(outputId)
      val exampleString = RequestJson.serialize(example.request)
      println(exampleString)
      outputId.newWriteChannelDisposable().useUp { writeChannel =>
        IOUtils.writeStringToChannel(exampleString, writeChannel)
      }
    }
  }

}
