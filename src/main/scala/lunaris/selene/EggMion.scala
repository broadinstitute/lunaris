package lunaris.selene

import better.files.File
import lunaris.app.VepSettings
import lunaris.genomics.Hg
import lunaris.vep.VepJobFiles

object EggMion {
  private def replace_file_name(fileName: String): Mion.Expression = {
    Mion.id("replace_file_name").call(Seq(
      Mion.id("path").assign(Mion.id("file_for_chrom")),
      Mion.id("file_name").assign(Mion.str(fileName)))
    )
  }

  def script(inputFile: File, vepJobFiles: VepJobFiles, cacheFile: File, colRef: String, colAlt: String,
             vepSettings: VepSettings, hg: Hg): Mion.Script = {
    val outputFileName = "extracted_data.tsv"
    val missesFileName = "cache_misses.vcf"
    val hgSettings = vepSettings.hgSettings(hg)
    Mion(
      Mion.id("files_by_chrom").assign(Mion.id("split_by_chrom").call(Seq(
        Mion.id("input_file").assign(Mion.str(inputFile.toString)),
        Mion.id("output_folder").assign(Mion.str(vepJobFiles.jobFolder.toString))
      ))),
      Mion.id("vep_and_cache_results").assign(
        Mion.id("file_for_chrom").iter(Mion.id("files_by_chrom")).scatter(
          Mion.block(
            Mion.id("tabix_outputs").assign(Mion.id("tabix").call(Seq(
              Mion.id("cache_file").assign(Mion.str(cacheFile.toString)),
              Mion.id("input_file").assign(Mion.id("file_for_chrom")),
              Mion.id("col_ref").assign(Mion.str(colRef)),
              Mion.id("col_alt").assign(Mion.str(colAlt)),
              Mion.id("output_file").assign(replace_file_name(outputFileName)),
              Mion.id("misses_file").assign(replace_file_name(missesFileName)),
              Mion.id("regions_file").assign(Mion.str(hgSettings.exonsFile.toString()))
            ))),
            Mion.id("vep_outputs").assign(
              Mion.id("vep").call(Seq(
                Mion.id("vep_cmd").assign(Mion.str(vepSettings.runSettings.vepCmd)),
                Mion.id("input_file")
                  .assign(Mion.id("tabix_outputs").member(Mion.id("misses_file"))),
                Mion.id("assembly").assign(Mion.str(hg.grcName)),
                Mion.id("fasta_file").assign(Mion.str(hgSettings.fastaFile.toString())),
                Mion.id("cache_dir").assign(Mion.str(vepSettings.runSettings.cacheDir.toString())),
                Mion.id("plugins_dir").assign(Mion.str(vepSettings.runSettings.pluginsDir.toString())),
                Mion.id("dbnsfp").assign(Mion.str(hgSettings.dbNSFPFile.toString())),
                Mion.id("output_file").assign(replace_file_name("vep_output")),
                Mion.id("warnings_file").assign(replace_file_name("vep_warnings")),
              ))
            ),
            Mion.id("vep_results_transformed").assign(
              Mion.id("transform_vep_results").call(Seq(
                Mion.id("input_file")
                  .assign(Mion.id("vep_outputs").member(Mion.id("output_file"))),
                Mion.id("output_file").assign(replace_file_name("vep_picked.tsv"))
              ))
            ),
            Mion.id("merge_outputs").assign(
              Mion.id("merge_sorted_files").call(Seq(
                Mion.id("input_file1")
                  .assign(Mion.id("tabix_outputs").member(Mion.id("output_file"))),
                Mion.id("pos_col1").assign(Mion.str(vepSettings.vepDataFieldsSettings.pos)),
                Mion.id("input_file2")
                  .assign(Mion.id("vep_results_transformed").member(Mion.id("output_file"))),
                Mion.id("pos_col2").assign(Mion.str(vepSettings.vepDataFieldsSettings.pos)),
                Mion.id("output_file").assign(replace_file_name("merged.tsv"))
              ))
            ),
            Mion.id("new").call(Seq(
              Mion.id("cache_misses_file")
                .assign(Mion.id("tabix_outputs").member(Mion.id("misses_file"))),
              Mion.id("merge_output_file")
                .assign(Mion.id("merge_outputs").member(Mion.id("output_file"))),
            ))
          )
        )
      ),
      Mion.id("merge_all_files").call(Seq(
        Mion.id("file_list").assign(Mion.id("new").call(Seq(
          Mion.id("cache_misses_file").assign(Mion.str(vepJobFiles.cacheMissesFile.toString)),
          Mion.id("merge_output_file").assign(Mion.str(vepJobFiles.mergedFile.toString()))
        ))),
        Mion.id("shards").assign(Mion.id("vep_and_cache_results"))
      ))
    )
  }
}
