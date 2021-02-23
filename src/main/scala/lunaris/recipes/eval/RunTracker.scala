package lunaris.recipes.eval

final class RunTracker(val snagTracker: SnagTracker, val statsTracker: StatsTracker = new StatsTracker) {

}

object RunTracker {
  def apply(snagTracker: SnagTracker): RunTracker = new RunTracker(snagTracker)
}
