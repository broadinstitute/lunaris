package lunaris.recipes.eval

trait LunRunnable {
  def execute(context: LunRunContext): Unit
}

object LunRunnable {

  def combine(runnables: Iterable[LunRunnable]): LunRunnable = {
    runnables.size match {
      case 0 => NoOpRunnable
      case 1 => runnables.head
      case _ => CompositeRunnable(runnables)
    }
  }

  object NoOpRunnable extends LunRunnable {
    override def execute(context: LunRunContext): Unit = ()
  }

  case class CompositeRunnable(runnables: Iterable[LunRunnable]) extends LunRunnable {
    override def execute(context: LunRunContext): Unit = runnables.foreach(_.execute(context))
  }
}

