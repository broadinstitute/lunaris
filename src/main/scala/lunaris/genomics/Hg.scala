package lunaris.genomics

import org.broadinstitute.yootilz.core.snag.Snag

sealed trait Hg {
  def name: String
}

object Hg {
  object Hg19 extends Hg {
    override val name: String = "hg19"
  }

  object Hg38 extends Hg {
    override val name: String = "hg38"
  }

  def parse(string: String): Either[Snag, Hg] = {
    string match {
      case Hg19.name => Right(Hg19)
      case Hg38.name => Right(Hg38)
      case _ => Left(Snag(s"Unknown human genome build $string."))
    }
  }
}
