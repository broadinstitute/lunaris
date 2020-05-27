package lunaris.io.request.examples

import lunaris.recipes.values.LunValue.PrimitiveValue.FileValue

object RequestExamplesUtils {
  object PortalData {
    val folder: String = "gs://fc-6fe31e1f-2c36-411c-bf23-60656d621184/data/t2d/"

    val associations: FileValue = FileValue(folder + "associations.tsv.gz")
    val associationsDkd: FileValue = FileValue(folder + "associations.dkd.tsv.gz")
    val posteriors: FileValue = FileValue(folder + "posteriors.tsv.gz")
    val regions: FileValue = FileValue(folder + "regions.tsv.gz")
    val variants: FileValue = FileValue(folder + "variants.tsv.gz")

  }


}
