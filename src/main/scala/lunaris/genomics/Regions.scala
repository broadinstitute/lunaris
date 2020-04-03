package lunaris.genomics

case class Regions(regionsByChromosome: Map[Chromosome, Seq[Region]])
