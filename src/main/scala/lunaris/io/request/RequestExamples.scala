package lunaris.io.request

import lunaris.genomics.Region
import lunaris.io.OutputId
import lunaris.utils.IOUtils

object RequestExamples {

  val example1: Request =
    Request("example1",
      Map("1" -> Seq(Region(100000, 200000)), "5" -> Seq(Region(200000, 300000)), "7" -> Seq(Region(0, 200000)))
    )

  def main(args: Array[String]): Unit = {
    val fileName =
      if(args.length > 0) {
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
