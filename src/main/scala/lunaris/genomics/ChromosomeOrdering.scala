package lunaris.genomics

import scala.util.control.NonFatal

object ChromosomeOrdering {
  val stringOrdering: Ordering[String] = Ordering.String

  val numericFirst: Ordering[String] = new Ordering[String] {
    private def parseInt(string: String): Option[Int] = {
      try {
        Some(string.toInt)
      } catch {
        case NonFatal(ex) => throw ex
      }
    }

    private def numericOpt(chrom: String): Option[Int] = {
      if (chrom.forall(_.isDigit)) {
        parseInt(chrom)
      } else if (chrom.startsWith("chr")) {
        val chrom2 = chrom.substring(3)
        parseInt(chrom2)
      } else {
        None
      }
    }

    override def compare(chrom1: String, chrom2: String): Int = {
      (numericOpt(chrom1), numericOpt(chrom2)) match {
        case (Some(number1), Some(number2)) => number1 - number2
        case (Some(_), None) => -1
        case (None, Some(_)) => 1
        case (None, None) => chrom1.compareTo(chrom2)
      }
    }
  }
}
