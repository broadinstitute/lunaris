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

  def run[A](query: Statement => A): Either[Snag, A] = {
    var connOpt: Option[Connection] = None
    try {
      val conn = dataSource.getConnection()
      connOpt = Some(conn)
      val stmt = conn.createStatement()
      Right(query(stmt))
    } catch {
      case NonFatal(exception) => Left(Snag(exception))
    } finally {
      connOpt.foreach(_.close())
    }
  }

  def runQuery[A, B](query: MushaQuery.WithResultSet[A])(consumer: A => B): Either[Snag, B] = {
    run(stmt => consumer(query(stmt)))
  }

  def runSingleResultQuery[A](query: MushaQuery.SingleResultMapping[A]): Either[Snag, A] = run(query).flatten

  def runUpdate(update: MushaQuery.UpdateWithoutCount): Either[Snag, Unit] = {
    run(update)
  }

  def runUpdate(update: MushaQuery.UpdateWithCount): Either[Snag, Int] = {
    run(update)
  }

  override def close(): Unit = dataSource.close()
}

object Musha {

  def apply(config: MushaConfig): Musha = new Musha(config)

  class ResultsWithMeta[A, B](val meta: A, val iterator: MushaIterator[B])

}