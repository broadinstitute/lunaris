package lunaris.datagen.app

import java.io.PrintWriter

import better.files.File
import lunaris.genomics.{HumanChromosomes, Locus}
import lunaris.utils.AscendingLongIterator

import scala.util.Random

/**
 * Creates simulated location-sorted TSV data file
 * Block-gzip with bgzip
 * Then index using tabix:
 * tabix -s1 -b2 -e2 <filename>
 */
object DataSimulator {

  case class LocusWithApsPos(locus: Locus, absPos: Long)

  case class Col(name: String, fieldGen: LocusWithApsPos => String)

  val random = new Random()

  def pickFrom(lots: Seq[String]): String = lots(random.nextInt(lots.size))

  val days = Seq("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
  val seasons = Seq("Spring", "Summer", "Fall", "Winter")
  val colors = Seq("blue", "red", "green", "yellow", "orange", "white", "black", "cyan", "magenta", "gray")
  val flavors = Seq("vanilla", "chocolate", "strawberry")

  def main(args: Array[String]): Unit = {
    val nRecords: Long = args(0).toLong
    val absPosIter = AscendingLongIterator(nRecords, HumanChromosomes.totalSize)
    val out = if(args.length > 1) {
      val outFile = File(args(1))
      outFile.newPrintWriter(autoFlush = false)
    } else {
      new PrintWriter(System.out)
    }
    val cols: Seq[Col] = Seq(
      Col("chrom", _.locus.chromosome.asString),
      Col("pos", _.locus.pos.toString),
      Col("absPos", _.absPos.toString),
      Col("MAF", _ => (random.nextDouble() * random.nextDouble()).toString),
      Col("p-value", _ => (random.nextDouble() * random.nextDouble() * random.nextDouble()).toString),
      Col("Z", _ => random.nextGaussian().toString),
      Col("cool", _ => random.nextBoolean().toString),
      Col("age", _ => random.nextInt(1 + random.nextInt(1 + random.nextInt(120))).toString),
      Col("day", _ => pickFrom(days)),
      Col("fingers", _ => "10"),
      Col("season", _ => pickFrom(seasons)),
      Col("color", _ => pickFrom(colors)),
      Col("flavor", _ => pickFrom(flavors)),
      Col("mystery", _ => (19 + random.nextInt(19 + random.nextInt(19 + random.nextInt(19)))).toString)
    )
    out.println("#" + cols.map(_.name).mkString("\t"))
    while (absPosIter.hasNext) {
      val absPos = absPosIter.next()
      val locus = HumanChromosomes.absPosToLocus(absPos).get
      val locusWithApsPos = LocusWithApsPos(locus, absPos)
      out.println(cols.map(_.fieldGen(locusWithApsPos)).mkString("\t"))
    }
    out.flush()
  }

}
