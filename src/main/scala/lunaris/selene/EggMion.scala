package lunaris.selene

import better.files.File

object EggMion {
  def script(inputFile: File, outputFolder: File, cacheFile: File, regionsFileOpt: Option[File], colRef: String,
             colAlt: String, outputFile: File, missesFile: File): Mion.Script = {
    val outputFileName = "extracted_data.tsv"
    val missesFileName = "cache_misses.vcf"
    val tabixArgsNoRegions = Seq(
      Mion.id("cache_file").assign(Mion.str(cacheFile.toString)),
      Mion.id("input_file").assign(Mion.id("file_for_chrom")),
      Mion.id("col_ref").assign(Mion.str(colRef)),
      Mion.id("col_alt").assign(Mion.str(colAlt)),
      Mion.id("output_file_name").assign(Mion.str(outputFileName)),
      Mion.id("misses_file_name").assign(Mion.str(missesFileName))
    )
    val tabixArgs = regionsFileOpt match {
      case Some(regionsFile) =>
        tabixArgsNoRegions :+ Mion.id("regions_file").assign(Mion.str(regionsFile.toString))
      case None =>
        tabixArgsNoRegions
    }
    Mion(
      Mion.id("files_by_chrom").assign(Mion.id("split_by_chrom").call(Seq(
        Mion.id("input_file").assign(Mion.str(inputFile.toString)),
        Mion.id("output_folder").assign(Mion.str(outputFolder.toString))
      ))),
      Mion.id("cache_match_results").assign(
        Mion.id("file_for_chrom").iter(Mion.id("files_by_chrom")).scatter(
          Mion.block(
            Mion.id("tabix").call(tabixArgs)
          )
        )
      ),
      Mion.id("merge_all_files").call(Seq(
        Mion.id("file_list").assign(Mion.id("new").call(Seq(
          Mion.id("output_file").assign(Mion.str(outputFile.toString)),
          Mion.id("misses_file").assign(Mion.str(missesFile.toString))
        ))),
        Mion.id("shards").assign(Mion.id("cache_match_results"))
      ))
    )
  }
}
