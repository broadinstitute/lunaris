package lunaris.vep

object VepMasksManager {

  val maskNames: Seq[String] = Seq("0of5_1pct")

  def getPathForMask(maskName: String): String = s"web/masks/$maskName.lun"

}
