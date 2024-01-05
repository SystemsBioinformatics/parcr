## code to prepare `fastafile` dataset goes here

# modified from https://bioinformatics.org/annhyb/examples/seq_fasta.html

fastafile <- c(
  ">sequence_A",
  toupper("ggtaagtcctctagtacaaacacccccaat"),
  toupper("tctgttgccagaaaaaacacttttaggcta"),
  "",
  ">sequence_B",
  toupper("attgtgatataattaaaattatattcatat"),
  toupper("tattagagccatcttctttgaagcgttgtc"),
  toupper("tatgcatcgatcgacgactg")
)

usethis::use_data(fastafile, overwrite = TRUE)
