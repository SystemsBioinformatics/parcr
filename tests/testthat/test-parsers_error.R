

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
# # # introduce error
# fastafile2 <- fastafile
# fastafile2[3] <- "TTGCAYTTCC"
#
# Parser(Fastafile())(fastafile2)
