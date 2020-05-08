package lunaris.data

import lunaris.data

case class DataSources(data: BlockGzippedWithIndex)

object DataSources {

  object Sim {
    val simDataOnTerra: BlockGzippedWithIndex =
      BlockGzippedWithIndex("gs://fc-6fe31e1f-2c36-411c-bf23-60656d621184/data/sim/sim.tsv.gz")

    val simDataOnOliversOldLaptop: BlockGzippedWithIndex =
      BlockGzippedWithIndex("/home/BROAD.MIT.EDU/oliverr/lunaris/data/sim/sim.tsv.gz")
  }

  object T2D {
    val folder: String = "gs://fc-6fe31e1f-2c36-411c-bf23-60656d621184/data/t2d/"
    val associationsDkd: BlockGzippedWithIndex = BlockGzippedWithIndex(folder + "associations.dkd.tsv.gz")
    val associations: BlockGzippedWithIndex = BlockGzippedWithIndex(folder + "associations.tsv.gz")
    val posteriors: BlockGzippedWithIndex = BlockGzippedWithIndex(folder + "posteriors.tsv.gz")
    val regions: BlockGzippedWithIndex = BlockGzippedWithIndex(folder + "regions.tsv.gz")
    val variants: BlockGzippedWithIndex = BlockGzippedWithIndex(folder + "variants.tsv.gz")
  }
}
