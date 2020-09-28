package lunaris.vep

import better.files.File

trait VepInstallation {
  def fastaFile: File
  def pluginsDir: File
  def dbNSFP: File
}

object VepInstallation {
  def autoPick: VepInstallation = OnBroadLaptopOliverUbuntu

  object OnBroadLaptopOliverUbuntu extends VepInstallation {
    val oliverHomeDir: File = File("/home/BROAD.MIT.EDU/oliverr")
    val auxDir: File = oliverHomeDir / "lunaris" / "vep" / "aux"
    override val fastaFile: File = auxDir / "Homo_sapiens.GRCh37.dna.primary_assembly.fa.bgz"
    override val pluginsDir: File = auxDir / "plugins"
    override val dbNSFP: File = auxDir / "dbNSFP3.5a_hg19.gz"
  }
}
