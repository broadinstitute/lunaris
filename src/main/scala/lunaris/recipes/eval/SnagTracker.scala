package lunaris.recipes.eval

import org.broadinstitute.yootilz.core.snag.Snag

class SnagTracker(val listeners: Seq[Snag => Unit]) {
  private val snagsBuilder = Seq.newBuilder[Snag]

  def trackSnag(snag: Snag): Unit = {
    listeners.foreach(_.apply(snag))
    snagsBuilder += snag
  }

  def buildSeq(): Seq[Snag] = snagsBuilder.result()

  def clear(): Unit = snagsBuilder.clear()

  def popSnags(): Seq[Snag] = {
    val snags = buildSeq()
    clear()
    snags
  }
}

object SnagTracker {
  def briefConsolePrinting: SnagTracker = new SnagTracker(Seq(snag => println(snag.message)))
}
