package musha

import com.zaxxer.hikari.{HikariConfig, HikariDataSource}
import org.broadinstitute.yootilz.core.snag.Snag
import org.h2.Driver

import java.io.Closeable
import java.sql.{Connection, DriverManager, Statement}
import scala.util.control.NonFatal

class Musha(config: MushaConfig) extends AutoCloseable with Closeable {
  DriverManager.registerDriver(new Driver)
  private val hikariConfig = new HikariConfig()
  private val dbFilePath = config.dbFilePath
  hikariConfig.setJdbcUrl(s"jdbc:h2:$dbFilePath")
  hikariConfig.setUsername(config.user)
  hikariConfig.setPassword(config.password)
  hikariConfig.addDataSourceProperty("cachePrepStmts", "true")
  hikariConfig.addDataSourceProperty("prepStmtCacheSize", "250")
  hikariConfig.addDataSourceProperty("prepStmtCacheSqlLimit", "2048")
  val dataSource = new HikariDataSource(hikariConfig)

  def runQuery[A, B](query: Statement => A)(consumer: A => B): Either[Snag, B] = {
    var connOpt: Option[Connection] = None
    try {
      val conn = dataSource.getConnection()
      connOpt = Some(conn)
      val stmt = conn.createStatement()
      Right(consumer(query(stmt)))
    } catch {
      case NonFatal(exception) => Left(Snag(exception))
    } finally {
      connOpt.foreach(_.close())
    }
  }
//  def runUpdate()  //  TODO
  override def close(): Unit = dataSource.close()
}

object Musha {

  class ResultsWithMeta[A, B](val meta: A, val iterator: MushaIterator[B])

}