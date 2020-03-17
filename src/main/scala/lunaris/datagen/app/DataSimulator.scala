package lunaris.datagen.app

import lunaris.genomics.HumanChromosomes
import lunaris.utils.AscendingLongIterator

object DataSimulator {

  def main(args: Array[String]): Unit = {
    val nRecords: Long = args(0).toLong
    val absPosIter = AscendingLongIterator(nRecords, HumanChromosomes.totalSize)
    println("chrom\tpos\tabsPos")
    while(absPosIter.hasNext) {
      val absPos = absPosIter.next()
      val locus = HumanChromosomes.absPosToLocus(absPos).get
      println(locus.chromosome.asString + "\t" + locus.pos + "\t" + absPos)
    }
  }

}
