test_that("'nofail' works in standard cases", {
  expect_error(nofail(literal("A"))("B"))
  expect_error(nofail(literal("A"))(character(0)))
  expect_equal(nofail(literal("A"))("A"), list(L=list("A"), R=character(0)))
  expect_error((literal("A") %then% nofail(literal("B")))(c("A","C")))

})

test_that("The linecounter in 'Parser' works.", {

})

# Define a parser for fasta files

Fastafile <- function() {
  one_or_more(SequenceBlock()) %then%
    eof()
}

SequenceBlock <- function() {
  MaybeEmpty() %then%
    Header() %then%
    Sequence() %using% function(x) list(x)
}

Sequence <- function() {
  one_or_more(SequenceString()) %using% function(x) list(sequence = paste(x, collapse=""))
}

Header <- function() {
  match_s(parse_header) %using% function(x) list(title = unlist(x))
}

SequenceString <- function() {
  match_s(parse_sequence_line)
}

parse_header <- function(line) {
  m <- stringr::str_match(line, "^>(\\w+)")
  if (is.na(m[1])) list() else m[2]
}

parse_sequence_line <- function(line) {
  m <- stringr::str_match(line, "^([GATC]+)$")
  if (is.na(m[1])) list() else m[2]
}

data(fastafile)

Fastafile()(fastafile)

# introduce error
fastafile2 <- fastafile
fastafile2[3] <- "TTGCAYTTCC"

Fastafile()(fastafile2)
