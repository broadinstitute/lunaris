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
    val job1 = getValue(db.newSubmittedJob(inputFile))
    val status1 = VepFileManager.ResultStatus.createSucceeded(job1.ctime, System.currentTimeMillis(), Seq.empty)
    val jobCopy1 = getValue(db.getJob(job1.id))
    assert(job1 == jobCopy1)
    getValue(db.updateJobStatus(job1.id, status1))
    val jobUpdated1 = getValue(db.getJob(job1.id))
    db.close()
    testDir.delete()
  }
}
