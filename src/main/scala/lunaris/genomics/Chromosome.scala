package lunaris.genomics

trait Chromosome {
  def asString: String

}

object Chromosome {

  case class Autosome(number: Int) extends Chromosome {
    override def asString: String = number.toString
  }

  case class Allosome(letter: Char) extends Chromosome {
    override def asString: String = letter.toString
  }

  def parse(string: String): Option[Chromosome] = {
    val stringTrimmed = if (string.startsWith("chr")) {
      string.substring(3)
    } else {
      string
    }
    if (stringTrimmed.forall(_.isDigit)) {
      Some(Autosome(stringTrimmed.toInt))
    } else if (stringTrimmed.length == 1) {
      Some(Allosome(stringTrimmed(0)))
    } else {
      None
    }
  }

  object ChromosomeOrdering extends Ordering[Chromosome] {
    override def compare(chr1: Chromosome, chr2: Chromosome): Int = {
      (chr1, chr2) match {
        case (Autosome(number1), Autosome(number2)) => number1 - number2
        case (Autosome(_), Allosome(_)) => 1
        case (Allosome(_), Autosome(_)) => -1
        case (Allosome(letter1), Allosome(letter2)) => letter1 - letter2
      }
    }
  }

}
