package lunaris.db

import scalikejdbc.interpolation.SQLSyntax
import scalikejdbc.{ConnectionPool, DB, withSQL}

final class DbAccess(url: String, user: String, password: String) {
  java.sql.DriverManager.registerDriver(new org.h2.Driver)
  ConnectionPool.singleton(url, user, password)

  DB.localTx { implicit session =>
    withSQL {
      ???  //  TODO: continue
    }
  }

  val testString: String = "yo"
}
