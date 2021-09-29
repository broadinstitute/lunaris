package lunaris.vep

import better.files.{File, Resource}
import lunaris.utils.IOUtils

class ScriptRepo(folder: File) {

  private val resourcePrefix: String = "lunaris/vep/"

  object Names {
    val sortVcf: String = "sort_vcf.sh"
  }

  object Files {
    val sortVcf: File = copyScriptToFolder(Names.sortVcf)
  }

  private def copyScriptToFolder(scriptName: String): File = {
    val file = folder / scriptName
    val resourceFullName = resourcePrefix + scriptName
    val fileOut = file.newOutputStream
    IOUtils.writeAllYouCanRead(Resource.getAsStream(resourceFullName), fileOut)
    file
  }
}

object ScriptRepo {
  def apply(folder: File): ScriptRepo = new ScriptRepo(folder)
}
