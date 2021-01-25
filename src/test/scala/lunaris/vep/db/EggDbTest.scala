package lunaris.vep.db

import better.files.File
import lunaris.vep.VepFileManager
import lunaris.vep.VepFileManager.ResultId
import org.broadinstitute.yootilz.core.snag.{Snag, SnagException}
import org.scalatest.funsuite.AnyFunSuite

class EggDbTest extends AnyFunSuite {
  private def getValue[A](snagOrValue: Either[Snag, A]): A = {
    snagOrValue match {
      case Left(snag) => throw new SnagException(snag)
      case Right(value) => value
    }
  }

  test("Store and retrieve records") {
    val testDir = File.newTemporaryDirectory()
    val workDir = testDir / "work"
    val dbFile = testDir / "eggtest"
    val outputFileForId = (id: ResultId) => workDir / (id.string + ".tsv")
    val db = EggDb(dbFile, outputFileForId)
    val inputFile = workDir / "my_input_file.vcf"
    val job = getValue(db.newSubmittedJob(inputFile))
    println(job)
    val status = VepFileManager.ResultStatus.createSucceeded(job.ctime, System.currentTimeMillis(), Seq.empty)
    val jobCopy = getValue(db.getJob(job.id))
    println(jobCopy)
    assert(job == jobCopy)
    getValue(db.updateJobStatus(job.id, status))
    val jobUpdated = getValue(db.getJob(job.id))
    println(jobUpdated)
    db.close()
    testDir.delete()
  }
}
