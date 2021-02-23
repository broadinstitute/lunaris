package lunaris.recipes.eval

import lunaris.recipes.eval.StatsTracker.Stats

final class StatsTracker {
  private var statsByName: Map[String, Stats] = Map.empty

  def addStats(name: String, cats: Seq[String]): Unit = {
    statsByName += (name -> Stats(cats))
  }

  def getStats(name: String): Option[Stats] = statsByName.get(name)

  def report(): Unit = {
    println("Stats:")
    for((name, stats) <- statsByName) {
      val totalCount = stats.getTotalCount
      val catsString = stats.cats.map { cat =>
        val nCat = stats.getCount(cat)
        val fCat = nCat.toDouble / totalCount
        val percentString = (100*fCat).toString.take(5)
        s"$nCat $cat ($percentString%)"
      }.mkString(", ")
      println(s"$totalCount $name: $catsString")
    }
    println("(done with stats)")
  }

  var reportLastTime: Long = System.currentTimeMillis()
  val reportTimeInterval: Long = 10000

  def maybeReport(): Unit = {
    val timeNow = System.currentTimeMillis()
    if(timeNow - reportLastTime > reportTimeInterval) {
      report()
      reportLastTime = timeNow
    }
  }
}

object StatsTracker {
  def apply(): StatsTracker = new StatsTracker

  final class Stats(val cats: Seq[String]) {
    private var totalCount: Long = 0
    private val counts: Array[Long] = new Array(cats.size)

    def add(cat: String): Unit = {
      val iCat = cats.indexOf(cat)
      if (iCat >= 0) {
        counts(iCat) += 1
      }
      totalCount += 1
    }

    def getCount(cat: String): Long = {
      val iCat = cats.indexOf(cat)
      if (iCat >= 0) counts(iCat) else 0
    }

    def getTotalCount: Long = totalCount

    def getFraction(cat: String): Double = getCount(cat).toDouble / totalCount
  }

  object Stats {
    def apply(cats: Seq[String]): Stats = new Stats(cats)
  }

}
