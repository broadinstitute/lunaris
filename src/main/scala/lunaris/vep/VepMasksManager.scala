package lunaris.vep

object VepMasksManager {

  val maskNames: Seq[String] = Seq("0of5_1pct", "1of5_1pct", "5of5_LoF_LC_1pct", "5of5", "11of11", "15of15", "LoF_HC")

  def getPathForMask(maskName: String): String = s"web/masks/$maskName.lun"

}
