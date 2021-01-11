package musha

import org.scalatest.funsuite.AnyFunSuite

class MushaTest extends AnyFunSuite {
  test("Hello") {
    val config = MushaConfig("/home/oliverr/lunaris/vep/work/h2/egg", "egg", "armeritter")
    val musha = new Musha(config)
    val query = MushaQuery[Int, String](Sql.ShowTables)(_.getColumnCount)((_, rs) => rs.getString(1))
    musha.runQuery(query) { iter =>
      println(s"Column count: ${iter.meta}, row count: ${iter.size}")
    }
    musha.close()
  }
}
