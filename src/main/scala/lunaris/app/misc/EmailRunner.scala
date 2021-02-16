package lunaris.app.misc

import lunaris.app.EmailSettings
import lunaris.utils.Crypt
import lunaris.vep.EmailManager

import scala.io.StdIn

object EmailRunner {
  def run(emailSettings: EmailSettings): Unit = {
    print("Please enter key to decode encrypted properties: ")
    val encryptionKey = StdIn.readLine()
    val crypt = Crypt(encryptionKey)
    val apiKey = crypt.decrypt(emailSettings.keyEncrypted)
    val emailManager = new EmailManager(emailSettings, apiKey)
    println("Now sending email")
    emailManager.sendTestMessage() match {
      case Left(snag) =>
        println("We hit a snag!")
        println(snag.report)
      case Right(response) =>
        println("Sent email. Result is:")
        println("HTTP " + response.getStatusCode)
        println(response.getHeaders)
        println(response.getBody)
    }
  }
}
