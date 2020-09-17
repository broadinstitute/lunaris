package lunaris.vep

import better.files.File

trait VepInstallation {
  def vepScriptFile: File
  def workDir: File
  def fastaFile: File
  def pluginsDir: File
  def dbNSFP: File
}

object VepInstallation {
  def autoPick: VepInstallation = OnBroadLaptopOliverUbuntu

  object OnBroadLaptopOliverUbuntu extends VepInstallation {
    val oliverHomeDir: File = File("/home/BROAD.MIT.EDU/oliverr")
    override val vepScriptFile: File = oliverHomeDir / "git" / "ensembl-vep" / "vep"
    override val workDir: File = oliverHomeDir / "lunaris" / "vep" / "work"
    val auxDir: File = oliverHomeDir / "lunaris" / "vep" / "aux"
    override val fastaFile: File = auxDir / "Homo_sapiens.GRCh37.dna.primary_assembly.fa.bgz"
    override val pluginsDir: File = auxDir / "plugins"
    override val dbNSFP: File = auxDir / "dbNSFP3.5a_hg19.gz"
  }
}
