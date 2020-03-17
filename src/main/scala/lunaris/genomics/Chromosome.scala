package lunaris.genomics

trait Chromosome {

}

object Chromosome {

  case class Autosome(number: Int) extends Chromosome

  case class Allosome(letter: Char) extends Chromosome

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
