package lunaris.db

import org.scalatest.funsuite.AnyFunSuite

class DbAccessTest extends AnyFunSuite{
  val url = "jdbc:h2:~/test"
  val user = "user"
  val password = "password"
  val dbAccess = new DbAccess(url, user, password)
  test("DB") {
    assert(dbAccess.testString == "yo")

  }

}
