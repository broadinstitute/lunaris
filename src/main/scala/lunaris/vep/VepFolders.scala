package lunaris.vep

import better.files.File
import lunaris.app.VepSettings
import lunaris.vep.VepJobManager.JobId
import org.broadinstitute.yootilz.core.snag.Snag

class VepFolders(vepSettings: VepSettings) {
  def inputsFolder: File = vepSettings.inputsFolder

  def resultsFolder: File = vepSettings.resultsFolder

  def workFolder: File = vepSettings.runSettings.workDir

  def runFolder: File = vepSettings.runSettings.runDir

  def vepJobFiles(jobId: JobId): VepJobFiles = VepJobFiles(this, jobId)

  private def folderOrSnag(folder: File, folderNick: String): Either[Snag, File] = {
    if (folder.exists && !folder.isDirectory) {
      Left(Snag(s"$folder should be folder, but is not."))
    } else if (!folder.exists) {
      folder.createDirectories()
      if (folder.exists) {
        Right(folder)
      } else {
        Left(Snag(s"Failed to create $folderNick $folder"))
      }
    } else {
      Right(resultsFolder)
    }
  }

  def foldersExistOrSnag(): Either[Snag, Unit] = {
    for {
      _ <- folderOrSnag(inputsFolder, "inputs folder")
      _ <- folderOrSnag(workFolder, "work folder")
      _ <- folderOrSnag(runFolder, "run folder")
      _ <- folderOrSnag(resultsFolder, "results folder")
    } yield ()
  }
}

object VepFolders {
  def apply(vepSettings: VepSettings): VepFolders = new VepFolders(vepSettings)
}
