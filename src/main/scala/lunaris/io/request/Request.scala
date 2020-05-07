package lunaris.io.request

import lunaris.genomics.Region

case class Request(id: String, regions: Map[String, Seq[Region]])
