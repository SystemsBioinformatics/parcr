p1 <- function(x) {(literal("A") %thenE% literal("B"))(x)}
p2 <- function(x) {(literal("A") %thenE% literal("C") %thenE% literal("D"))(x)}

test_that("'Parser' works", {
  #expect_error(Parser(literal("A"))("B"), regexp="line 1")
  expect_equal(Parser(p1)(c("A","B")), list(L=c(list("A"), list("B")), R=character(0)))
  expect_equal(Parser(p2)(c("A","C","D")), list(L=c(list("A"), list("C"), list("D")), R=character(0)))
  expect_error(Parser(p1)(c("A","C")), regexp="line 2")
  expect_error(Parser(p2)(c("A","C","E")), regexp="line 3")
  expect_equal(Parser((literal("B") %orE% literal("A")))(c("A","B")), list(L=c(list("A")), R="B"))
  expect_error(Parser(literal("A") %orE% literal("B"))("C"), regexp = "line 1")
  expect_equal(Parser(p1 %orE% p2)(c("A","C","D")), list(L=c(list("A"), list("C"), list("D")), R=character(0)))
  expect_error(Parser(p2 %orE% p1)(c("A","C","E")), regexp = "line 3")
  expect_error(Parser(p1 %orE% p2)(c("A","C","E")), regexp = "line 3")
})


# Define a parser for fasta files


# Fastafile <- function() {
#   one_or_more(SequenceBlock()) %then%
#     eof()
# }
#
# SequenceBlock <- function() {
#   MaybeEmpty() %then%
#     Header() %then%
#     Sequence() %using% function(x) list(x)
# }
#
# Sequence <- function() {
#   one_or_more(SequenceString()) %using% function(x) list(sequence = paste(x, collapse=""))
# }
#
# Header <- function() {
#   match_s(parse_header) %using% function(x) list(title = unlist(x))
# }
#
# SequenceString <- function() {
#   match_s(parse_sequence_line)
# }
#
# parse_header <- function(line) {
#   m <- stringr::str_match(line, "^>(\\w+)")
#   if (is.na(m[1])) list() else m[2]
# }
#
# parse_sequence_line <- function(line) {
#   m <- stringr::str_match(line, "^([GATC]+)$")
#   if (is.na(m[1])) list() else m[2]
# }
#
# data(fastafile)
#
# Fastafile()(fastafile)
#
# # introduce error
# fastafile2 <- fastafile
# fastafile2[3] <- "TTGCAYTTCC"
#
# Fastafile()(fastafile2)
