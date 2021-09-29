package lunaris.genomics

import org.broadinstitute.yootilz.core.snag.Snag

sealed trait Hg {
  def name: String
  def grcName: String
}

object Hg {
  object Hg19 extends Hg {
    override val name: String = "hg19"

    override val grcName: String = "GRCh37"
  }

  object Hg38 extends Hg {
    override val name: String = "hg38"

    override val grcName: String = "GRCh38"
  }

  def parse(string: String): Either[Snag, Hg] = {
    string match {
      case Hg19.name | Hg19.grcName => Right(Hg19)
      case Hg38.name | Hg38.grcName => Right(Hg38)
      case _ => Left(Snag(s"Unknown human genome build $string."))
    }
  }
}
