package lunaris.vep.db

import better.files.File
import lunaris.vep.VepFileManager
import lunaris.vep.VepFileManager.{JobId, ResultStatus, SessionId}
import lunaris.vep.db.EggDb.JobRecord
import org.broadinstitute.yootilz.core.snag.{Snag, SnagException}
import org.scalatest.funsuite.AnyFunSuite

import scala.util.Random

class EggDbTest extends AnyFunSuite {
  private def getValue[A](snagOrValue: Either[Snag, A]): A = {
    snagOrValue match {
      case Left(snag) => throw new SnagException(snag)
      case Right(value) => value
    }
  }

  private def createMockJob(): JobRecord = {
    val jobId = JobId.createNew()
    val sessionId = SessionId.createNew()
    val inputFileClient = File("input_" + Random.nextInt(100) + ".vcf")
    val inputFileServer = File(jobId.string + ".vcf")
    val outputFile = File(jobId.string + ".tsv")
    val filter = ""
    val format = "rareMETALS"
    val submissionTime = System.currentTimeMillis()
    val resultStatus = ResultStatus.createSubmitted(submissionTime, Seq.empty)
    JobRecord(jobId, sessionId, inputFileClient, inputFileServer, outputFile, filter, format, resultStatus.statusType,
      resultStatus.message, resultStatus.snagMessages, submissionTime, submissionTime)
  }

  test("Store and retrieve records") {
    val testDir = File.newTemporaryDirectory()
    val workDir = testDir / "work"
    val dbFile = testDir / "eggtest"
    val inputFileForId = (id: JobId) => workDir / ("input_" + id.string + ".vcf")
    val outputFileForId = (id: JobId) => workDir / (id.string + ".tsv")
    val db = EggDb(dbFile, inputFileForId, outputFileForId)
    var jobIds: Set[JobId] = Set.empty
    val nJobs = 5
    for(_ <- 0 until nJobs) {
      val job = createMockJob()
      assert(getValue(db.getSessionOpt(job.sessionId)).isEmpty)
      getValue(db.insertJob(job))
      assert(getValue(db.getSessionOpt(job.sessionId)).nonEmpty)
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
