package lunaris.genomics

import org.scalatest.funsuite.AnyFunSuite

final class LociSetTest extends AnyFunSuite{
  private def assertContainments(regions: Seq[Region], nRegionsConsolidated: Int, posListContained: Seq[Int],
                                 posListNotContained: Seq[Int]): Unit = {
    val chrom = "2"
    val loci = regions.map(Locus(chrom, _))
    for(lociPermuted <- loci.permutations) {
      val builder = LociSet.newBuilder
      for(locus <- lociPermuted) {
        builder += locus
      }
      val lociSet = builder.result()
      assert(lociSet.size == nRegionsConsolidated)
      for(posContained <- posListContained) {
        assert(lociSet.contains(chrom, posContained))
      }
      for(posNotContained <- posListNotContained) {
        assert(!lociSet.contains(chrom, posNotContained))
      }
    }
  }

  test("Separate regions") {
    val regions = Seq(Region(100, 200), Region(300, 400), Region(500, 600))
    val nRegionsConsolidated = 3
    val posListContained = Seq(100, 150, 300, 350, 500, 550)
    val posListNotContained = Seq(0, 50, 200, 250, 400, 450, 600, 650)
    assertContainments(regions, nRegionsConsolidated, posListContained, posListNotContained)
  }

  test("Touching regions") {
    val regions = Seq(Region(100, 200), Region(200, 300), Region(300, 400))
    val nRegionsConsolidated = 1
    val posListContained = Seq(100, 150, 200, 250, 300, 350)
    val posListNotContained = Seq(0, 50, 400, 450, 500, 550, 600, 650)
    assertContainments(regions, nRegionsConsolidated, posListContained, posListNotContained)
  }

  test("Overlapping regions") {
    val regions = Seq(Region(100, 200), Region(150, 250), Region(200, 300))
    val nRegionsConsolidated = 1
    val posListContained = Seq(100, 150, 200, 250)
    val posListNotContained = Seq(0, 50, 300, 350, 400, 450, 500)
    assertContainments(regions, nRegionsConsolidated, posListContained, posListNotContained)
  }
}
