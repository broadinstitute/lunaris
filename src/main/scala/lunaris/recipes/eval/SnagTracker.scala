package lunaris.recipes.eval

import org.broadinstitute.yootilz.core.snag.Snag

class SnagTracker(val listeners: Seq[Snag => Unit], val nSnagsMax: Int = 100) {
  private var snagsBuilderPrevious = Seq.newBuilder[Snag]
  private var snagsBuilderCurrent = Seq.newBuilder[Snag]
  private var nSnagsInCurrent = 0

  def trackSnag(snag: Snag): Unit = {
    listeners.foreach(_.apply(snag))
    snagsBuilderCurrent += snag
    nSnagsInCurrent += 1
    if(nSnagsInCurrent >= nSnagsMax) {
      snagsBuilderPrevious = snagsBuilderCurrent
      snagsBuilderCurrent = Seq.newBuilder
      nSnagsInCurrent = 0
    }
  }

  def buildSeq(): Seq[Snag] = {
    (snagsBuilderPrevious.result() ++ snagsBuilderCurrent.result()).takeRight(nSnagsMax)
  }

  def clear(): Unit = {
    snagsBuilderPrevious.clear()
    snagsBuilderCurrent.clear()
    nSnagsInCurrent = 0
  }

  def popSnags(): Seq[Snag] = {
    val snags = buildSeq()
    clear()
    snags
  }
}

object SnagTracker {
  def briefConsolePrinting: SnagTracker = new SnagTracker(Seq(snag => println(snag.message)))
}
