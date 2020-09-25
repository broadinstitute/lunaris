package lunaris.utils

class BuilderBox[T](var value: T) {
  def modify(mod: T => T): Unit = {
    value = mod(value)
  }

  def modifyForeach[A](as: Iterable[A])(mod: (T, A) => T): Unit = {
    for(a <- as) {
      value = mod(value, a)
    }
  }
}

object BuilderBox {
  def apply[T](value: T): BuilderBox[T] = new BuilderBox[T](value)
}
