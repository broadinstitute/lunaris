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
    var jobIds: Set[ResultId] = Set.empty
    val nJobs = 5
    for(_ <- 0 until nJobs) {
      val job = getValue(db.newSubmittedJob(inputFile))
      val jobId = job.id
      jobIds += jobId
      val jobCopy = getValue(db.getJob(job.id))
      assert(job == jobCopy)
      val status = VepFileManager.ResultStatus.createSucceeded(job.ctime, System.currentTimeMillis(), Seq.empty)
      getValue(db.updateJobStatus(job.id, status))
      val jobUpdated = getValue(db.getJob(job.id))
      assert(job != jobUpdated)
      val nJobsInDb = getValue(db.countJobs())
      assert(jobIds.size == nJobsInDb)
    }
    while(jobIds.nonEmpty) {
      val jobId = jobIds.head
      val nJobsBefore = getValue(db.countJobs())
      assert(nJobsBefore == jobIds.size)
      getValue(db.deleteJob(jobId))
      jobIds -= jobId
      val nJobsAfter = getValue(db.countJobs())
      assert(nJobsAfter == jobIds.size)
    }
    db.close()
    testDir.delete()
  }
}
