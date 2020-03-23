package lunaris.data

object DataSources {

  val simDataOnTerra: DataSourceWithIndex =
    DataSourceWithIndex(
      "gs://fc-6fe31e1f-2c36-411c-bf23-60656d621184/data/sim/sim.tsv.gz",
      "gs://fc-6fe31e1f-2c36-411c-bf23-60656d621184/data/sim/sim.tsv.gz.tbi"
    )

  val simDataOnOliversOldLaptop: DataSourceWithIndex =
    DataSourceWithIndex(
      "/home/BROAD.MIT.EDU/oliverr/lunaris/data/sim/sim.tsv.gz",
      "/home/BROAD.MIT.EDU/oliverr/lunaris/data/sim/sim.tsv.gz.tbi"
    )

}
