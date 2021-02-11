package lunaris.vep

import com.sendgrid.{Method, Request, Response, SendGrid}
import com.sendgrid.helpers.mail.Mail
import com.sendgrid.helpers.mail.objects.{Content, Email}
import lunaris.app.EmailSettings
import org.broadinstitute.yootilz.core.snag.Snag

import scala.util.control.NonFatal

class EmailManager(emailSettings: EmailSettings, apiKey: String) {
  private val sendGrid = new SendGrid(apiKey)

  def sendTestMessage(): Either[Snag, Response] = {
    val from = new Email("oliverr@broadinstitute.org")
    val subject = "Test message from EGG Server"
    val to = new Email("oliverr@broadinstitute.org")
    val content = new Content("text/plain", "Hello, it is me, the EGG Server,")
    val mail = new Mail(from, subject, to, content)
    val request = new Request()
    try {
      request.setMethod(Method.POST)
      request.setEndpoint("mail/send")
      request.setBody(mail.build)
      val response = sendGrid.api(request)
      println(response.getStatusCode)
      println(response.getBody)
      println(response.getHeaders)
      Right(response)
    } catch {
      case NonFatal(exception) => Left(Snag(exception))
    }
  }
}

object EmailManager {
  def apply(emailSettings: EmailSettings, apiKey: String): EmailManager = new EmailManager(emailSettings, apiKey)
}

