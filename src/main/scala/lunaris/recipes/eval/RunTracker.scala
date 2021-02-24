package lunaris.recipes.eval

final class RunTracker(val snagTracker: SnagTracker, val statsTracker: StatsTracker = new StatsTracker(println)) {

}

object RunTracker {
  def apply(snagTracker: SnagTracker, statsTracker: StatsTracker = new StatsTracker(println)): RunTracker =
    new RunTracker(snagTracker, statsTracker)
}
