package lunaris.varianteffect

import better.files.File
import org.broadinstitute.yootilz.core.snag.Snag

class ResultFileManager(val resultFolder: File) {

  def resultsFolderOrSnag(): Either[Snag, File] = {
    if(resultFolder.exists && !resultFolder.isDirectory) {
      Left(Snag(s"$resultFolder should be folder, but is not."))
    } else if(!resultFolder.exists) {
      resultFolder.createDirectory()
      if(resultFolder.exists) {
        Right(resultFolder)
      } else {
        Left(Snag(s"Failed to create $resultFolder"))
      }
    } else {
      Right(resultFolder)
    }
  }

//  def submit(fileName: String):

}
