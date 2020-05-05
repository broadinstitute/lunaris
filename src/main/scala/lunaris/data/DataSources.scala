package lunaris.data

import lunaris.io.InputId

case class DataSources(data: BlockGzippedWithIndex, bed: InputId)

object DataSources {

  val simDataOnTerra: DataSources =
    DataSources(
      data =
        BlockGzippedWithIndex(
          "gs://fc-6fe31e1f-2c36-411c-bf23-60656d621184/data/sim/sim.tsv.gz",
          "gs://fc-6fe31e1f-2c36-411c-bf23-60656d621184/data/sim/sim.tsv.gz.tbi"
        ),
      bed = InputId("gs://fc-6fe31e1f-2c36-411c-bf23-60656d621184/data/sim/sim.bed")
    )

  val simDataOnOliversOldLaptop: DataSources =
    DataSources(
      data =
        BlockGzippedWithIndex(
          "/home/BROAD.MIT.EDU/oliverr/lunaris/data/sim/sim.tsv.gz",
          "/home/BROAD.MIT.EDU/oliverr/lunaris/data/sim/sim.tsv.gz.tbi"
        ),
      bed = InputId("/home/BROAD.MIT.EDU/oliverr/lunaris/data/sim/sim.bed")
    )
}
