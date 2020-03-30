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

  val simDataOnLouisLaptop: DataSourceWithIndex =
    DataSourceWithIndex(
      "/Users/louisb/Workspace/lunaris/out.txt.gz",
      "/Users/louisb/Workspace/lunaris/out.txt.gz.tbi",
    )

  val simDataInPublicBucket: DataSourceWithIndex =
    DataSourceWithIndex(
      "gs://hellbender/test/resources/lunaris/out.txt.gz",
      "gs://hellbender/test/resources/lunaris/out.txt.gz.tbi"
    )

}
