package lunaris.genomics

object HumanChromosomes {

  val chr1: Chromosome.Autosome = Chromosome.Autosome(1)
  val chr2: Chromosome.Autosome = Chromosome.Autosome(2)
  val chr3: Chromosome.Autosome = Chromosome.Autosome(3)
  val chr4: Chromosome.Autosome = Chromosome.Autosome(4)
  val chr5: Chromosome.Autosome = Chromosome.Autosome(5)
  val chr6: Chromosome.Autosome = Chromosome.Autosome(6)
  val chr7: Chromosome.Autosome = Chromosome.Autosome(7)
  val chr8: Chromosome.Autosome = Chromosome.Autosome(8)
  val chr9: Chromosome.Autosome = Chromosome.Autosome(9)
  val chr10: Chromosome.Autosome = Chromosome.Autosome(10)
  val chr11: Chromosome.Autosome = Chromosome.Autosome(11)
  val chr12: Chromosome.Autosome = Chromosome.Autosome(12)
  val chr13: Chromosome.Autosome = Chromosome.Autosome(13)
  val chr14: Chromosome.Autosome = Chromosome.Autosome(14)
  val chr15: Chromosome.Autosome = Chromosome.Autosome(15)
  val chr16: Chromosome.Autosome = Chromosome.Autosome(16)
  val chr17: Chromosome.Autosome = Chromosome.Autosome(17)
  val chr18: Chromosome.Autosome = Chromosome.Autosome(18)
  val chr19: Chromosome.Autosome = Chromosome.Autosome(19)
  val chr20: Chromosome.Autosome = Chromosome.Autosome(20)
  val chr21: Chromosome.Autosome = Chromosome.Autosome(21)
  val chr22: Chromosome.Autosome = Chromosome.Autosome(22)
  val chrX: Chromosome.Allosome = Chromosome.Allosome('X')
  val chrY: Chromosome.Allosome = Chromosome.Allosome('Y')

  val all: List[Chromosome] =
    List(
      chr1, chr2, chr3, chr4, chr5, chr6, chr7, chr8, chr9, chr10, chr11, chr12, chr13, chr14, chr15, chr16, chr17,
      chr18, chr19, chr20, chr21, chr22, chrX, chrY
    )

  val sizes: Map[Chromosome, Long] =
    Map(
      chr1 -> 248956422, chr2 -> 242193529, chr3 -> 198295559, chr4 -> 190214555, chr5 -> 181538259,
      chr6 -> 170805979, chr7 -> 159345973, chr8 -> 145138636, chr9 -> 138394717, chr10 -> 133797422,
      chr11 -> 135086622, chr12 -> 133275309, chr13 -> 114364328, chr14 -> 107043718, chr15 -> 101991189,
      chr16 -> 90338345, chr17 -> 83257441, chr18 -> 80373285, chr19 -> 58617616, chr20 -> 64444167,
      chr21 -> 46709983, chr22 -> 50818468, chrX -> 156040895, chrY -> 57227415
    )

}
