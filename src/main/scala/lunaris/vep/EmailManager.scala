package lunaris.vep

import com.sendgrid.{Method, Request, Response, SendGrid}
import com.sendgrid.helpers.mail.Mail
import com.sendgrid.helpers.mail.objects.{Content, Email}
import lunaris.app.EmailSettings
import lunaris.recipes.eval.LunRunnable.RunResult
import lunaris.utils.DateUtils
import lunaris.vep.VepJobManager.{JobId, SessionId}
import org.broadinstitute.yootilz.core.snag.Snag

import java.util.Date
import scala.util.control.NonFatal

final class EmailManager(emailSettings: EmailSettings, apiKey: String) {
  private val sendGrid = new SendGrid(apiKey)
  private val from = new Email("eggserver@broadinstitute.org", "EGG Server")

  private def snagsToHtml(snags: Seq[Snag]): String = {
    if (snags.isEmpty) {
      ""
    } else {
      "<p>There were the following problems:</p>\n" + snags.take(100).map { snag =>
        s"<p>${snag.message}</p>\n"
      }.mkString("") + (if (snags.size > 100) s"<p>(and ${snags.size - 100} more)</p>\n" else "")
    }
  }

  def sendJobResultMessage(toAddress: String, submissionTime: Long, successTime: Long, jobId: JobId,
                           sessionId: SessionId, result: RunResult): Either[Snag, Response] = {
    val submissionDate = new Date(submissionTime)
    val successDate = new Date(successTime)
    val runTimeString = DateUtils.timeDiffToString(successTime - submissionTime)
    val subject = s"Job submitted on $submissionDate has succeeded."
    val summary =
      s"""
         |<p>Hello,</p>
         |
         |<p>Your job submitted to EGG Server on $submissionDate has successfully completed on $successDate
         |after $runTimeString.</p>
         |
         |<p>To get back to your session, click
         |<a href="http://eggserver.org/lunaris/vep.html?session=$sessionId">here</a>, or download the result
         |<a href="http://eggserver.org/lunaris/predictor/results/$jobId.tsv" download="$jobId.tsv">here</a>.
         |</p>
         |""".stripMargin
    val errorList = snagsToHtml(result.snags)
    val content = summary + errorList
    sendMessage(toAddress, subject, content)
  }

  def sendJobSnagMessage(toAddress: String, submissionTime: Long, failTime: Long, sessionId: SessionId, snag: Snag):
  Either[Snag, Response] = {
    val submissionDate = new Date(submissionTime)
    val failDate = new Date(failTime)
    val subject = s"Job submitted on $submissionDate has failed."
    val summary =
      s"""
         |<p>Hello,</p>
         |
         |<p>Your job submitted to EGG Server on $submissionDate has failed with an exception on $failDate.</p>
         |
         |<p>To get back to your session, click
         |<a href="http://eggserver.org/lunaris/vep.html?session=$sessionId">here</a>
         |""".stripMargin
    val stackTrace = "<pre>\n" + snag.report + "\n</pre>\n"
    val content = summary + stackTrace
    sendMessage(toAddress, subject, content)
  }

  def sendTestMessage(): Either[Snag, Response] = {
    val toAddress = "oliverr@broadinstitute.org"
    val subject = "Test message from EGG Server"
    val content =
      """
        |<p>Hello,<p>
        |<p>This is a test message from the
        |<a href="http://eggserver.org/lunaris/vep.html">EGG Server</a>.</p>
        |""".stripMargin
    sendMessage(toAddress, subject, content)
  }

  private def sendMessage(toAddress: String, subject: String, contentString: String): Either[Snag, Response] = {
    val to = new Email(toAddress)
    val content = new Content("text/html", contentString)
    val mail = new Mail(from, subject, to, content)
    val request = new Request()
    try {
      request.setMethod(Method.POST)
      request.setEndpoint("mail/send")
      request.setBody(mail.build)
      val response = sendGrid.api(request)
      Right(response)
    } catch {
      case NonFatal(exception) => Left(Snag(exception))
    }
  }
}

object EmailManager {
  def apply(emailSettings: EmailSettings, apiKey: String): EmailManager = new EmailManager(emailSettings, apiKey)
}

