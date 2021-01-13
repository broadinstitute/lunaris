package musha

import org.scalatest.funsuite.AnyFunSuite

class MushaTest extends AnyFunSuite {
  test("Hello") {
    val config = MushaConfig("/home/oliverr/lunaris/vep/work/h2/egg", "egg", "armeritter")
    val musha = new Musha(config)
    val query = MushaQuery.metaRowsIter[String](Sql.ShowTables) { (_, rs) =>
      rs.getString(1)
    }
    val snagOrUnit = musha.runQuery(query) { iter =>
      println(s"Column count: ${iter.metaData.columns.size}")
      println(s"Row count: ${iter.size}")
    }
    musha.close()
    snagOrUnit.left.foreach(snag => println(snag.report))
  }
}
