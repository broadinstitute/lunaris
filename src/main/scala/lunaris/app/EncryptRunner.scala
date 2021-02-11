package lunaris.app

import lunaris.utils.Crypt

import scala.io.StdIn

object EncryptRunner {
  def run(): Unit = {
    print("Enter key to use for encryption: ")
    val key = StdIn.readLine
    print("Enter property value to encrypt: ")
    val property = StdIn.readLine()
    val crypt = Crypt(key)
    val encryptedProperty = crypt.encrypt(property)
    println("= = = Encrypted property below this line = = =")
    println(encryptedProperty)
    println("= = = Encrypted property above this line = = =")
    println("Done!")
  }
}
