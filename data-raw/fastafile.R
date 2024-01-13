## code to prepare `fastafile` dataset goes here

fastafile <- c(
  ">sequence_A",
  toupper("ggtaagtcctctagtacaaacacccccaat"),
  toupper("tctgttgccagaaaaaacacttttaggcta"),
  ">sequence_B",
  toupper("attgtgatataattaaaattatattcatat"),
  toupper("tattagagccatcttctttgaagcgttgtc"),
  toupper("tatgcatcgatc"),
  "",
  ">sequence_C",
  "MTEITAAMVKELRESTGAGMMDCKNALSET",
  "NGDFDKAVQLLREKGLGKAAKKADRLAAEG",
  "ENEYKALVAELEKE"
)

usethis::use_data(fastafile, overwrite = TRUE)

