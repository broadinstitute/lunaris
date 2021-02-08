package lunaris.utils

import org.scalatest.funsuite.AnyFunSuite

class CryptTest extends AnyFunSuite{
  private def assertRoundTripped(string: String, crypt: Crypt): Unit = {
    val encrypted = crypt.encrypt(string)
    val decrypted = crypt.decrypt(encrypted)
    println(string + " => " + encrypted)
    assert(decrypted == string)
  }

  test("round trips") {
    val key = "awesome"
    val crypt = new Crypt(key)
    assertRoundTripped("Hello, world!", crypt)
    assertRoundTripped("Forty-Two", crypt)
    assertRoundTripped("Be always grateful, never satisfied.", crypt)
  }
}
