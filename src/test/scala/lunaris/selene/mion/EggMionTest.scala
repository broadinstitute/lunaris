package lunaris.selene.mion

import better.files.File
import lunaris.selene.EggMion
import lunaris.selene.mion.EggMionTest.{eggMion, reformat}
import org.scalatest.funsuite.AnyFunSuite

object EggMionTest {
  def reformat(string: String): String =
    string.trim.replaceAll("\\s+", " ")
      .replaceAll("\\( ", "(")
      .replaceAll(" \\)", ")")

  val eggMion: String =
    """
      |files_by_chrom = split_by_chrom(
      |     input_file = "/home/oliverr/lunaris/vep/test/inputs/sample_input_1e6.vcf",
      |     output_folder = "/home/oliverr/lunaris/vep/test/tmp"
      |);
      |cache_match_results = (file_for_chrom <- files_by_chrom) {
      |    tabix(
      |        cache_file = "/home/oliverr/lunaris/vep/data/all_sites.vep.tsv.gz",
      |        input_file = file_for_chrom,
      |        col_ref = "Ref",
      |        col_alt = "Alt",
      |        output_file_name = "extracted_data.tsv",
      |        misses_file_name = "cache_misses.vcf",
      |        regions_file = "/home/oliverr/lunaris/vep/aux/55k.gencode.transcript.exons"
      |    );
      |};
      |merge_all_files(
      |    file_list = new(
      |        output_file = "/home/oliverr/lunaris/vep/test/tmp/extracted_data.tsv",
      |        misses_file = "/home/oliverr/lunaris/vep/test/tmp/cache_misses.vcf"
      |    ),
      |    shards = cache_match_results
      |);
      |""".stripMargin
}

class EggMionTest extends AnyFunSuite {
  test("Generate egg.mion file.") {
    val inputFile = File("/home/oliverr/lunaris/vep/test/inputs/sample_input_1e6.vcf")
    val outputFolder = File("/home/oliverr/lunaris/vep/test/tmp")
    val cacheFile = File("/home/oliverr/lunaris/vep/data/all_sites.vep.tsv.gz")
    val regionsFile = File("/home/oliverr/lunaris/vep/aux/55k.gencode.transcript.exons")
    val colRef = "Ref"
    val colAlt = "Alt"
    val outputFile = File("/home/oliverr/lunaris/vep/test/tmp/extracted_data.tsv")
    val missesFile = File("/home/oliverr/lunaris/vep/test/tmp/cache_misses.vcf")
    val eggMionConstructed =
      EggMion.script(inputFile, outputFolder, cacheFile, Some(regionsFile), colRef, colAlt, outputFile, missesFile)
        .to_code()
    val constructedBlanked = reformat(eggMionConstructed)
    val expectedBlanked = reformat(eggMion)
    assert(constructedBlanked === expectedBlanked)
  }
}
