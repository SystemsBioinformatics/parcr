## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, echo=FALSE--------------------------------------------------------
library(parcr)

## -----------------------------------------------------------------------------
succeed("A")("abc")
succeed(data.frame(title="Keisri hull", author="Jaan Kross"))(c("Unconsumed","text"))

## -----------------------------------------------------------------------------
fail()("abc")

## -----------------------------------------------------------------------------
literal('abc')(c('abc','def'))

## -----------------------------------------------------------------------------
starts_with_a <- function(x) {grepl("^a", x)}
satisfy(starts_with_a)(c('abc','def'))

## -----------------------------------------------------------------------------
literal('a')(c('ab','a'))

## -----------------------------------------------------------------------------
(literal("a") %then% literal("att") %then% eof())(c("a","att"))

## -----------------------------------------------------------------------------
(literal("a") %then% literal("att"))(c("a","att"))

## -----------------------------------------------------------------------------
(literal('A') %or% satisfy(starts_with_a))(c('abc','def'))

## -----------------------------------------------------------------------------
(literal('A') %then% satisfy(starts_with_a))(c('A', 'abc'))

## -----------------------------------------------------------------------------
(literal('>') %thenx% satisfy(starts_with_a))(c('>', 'abc'))

## -----------------------------------------------------------------------------
(literal('a') %ret% "We have an 'a'")(c('a','b'))

## -----------------------------------------------------------------------------
(satisfy(starts_with_a) %using% toupper)(c('abc','d'))

## -----------------------------------------------------------------------------
zero_or_one(satisfy(starts_with_a))(c('acc','aat','cgg'))

## -----------------------------------------------------------------------------
zero_or_more(satisfy(starts_with_a))(c('cat','gac','cct'))

## -----------------------------------------------------------------------------
one_or_more(satisfy(starts_with_a))(c('att','aac','cct'))

## -----------------------------------------------------------------------------
exactly(2, satisfy(starts_with_a))(c('att','aac','cct'))

## -----------------------------------------------------------------------------
match_n(1, satisfy(starts_with_a))(c('att','aac','cct'))

## -----------------------------------------------------------------------------
numbers <- function(x) {
  m <- gregexpr("[[:digit:]]+", x)
  matches <- as.numeric(regmatches(x,m)[[1]])
  if (length(matches)==0) {
    return(list()) # we signal parser failure when no numbers were found
  } else {
    return(matches)
  }
}

match_s(numbers)(" 101 12 187 # a comment on these numbers")

## -----------------------------------------------------------------------------
starts_with_a <- function(x) grepl("^a",x[1])
# don's forget to use satisfy(), it turns starts_with into a parser
by_split(one_or_more(satisfy(starts_with_a)), ",", fixed = TRUE)("atggc,acggg,acttg")

## -----------------------------------------------------------------------------
by_symbol(literal(">") %thenx% one_or_more(literal("b")), finish = FALSE)(">bb")

## -----------------------------------------------------------------------------
EmptyLine()("")

## -----------------------------------------------------------------------------
Spacer()(c(" ","\t\t\t", "atgcc"))

## -----------------------------------------------------------------------------
MaybeEmpty()(c("ggacc","gatccg", "atgcc"))

## ----echo=FALSE, comment = NA-------------------------------------------------
data("fastafile")
cat(paste0(fastafile, collapse="\n"))

## ----eval=FALSE---------------------------------------------------------------
#  data("fastafile")

## -----------------------------------------------------------------------------
Fasta <- function() {
  one_or_more(SequenceBlock()) %then%
    eof()
}

SequenceBlock <- function() {
  MaybeEmpty() %then% 
    Header() %then% 
    (NuclSequence() %or% ProtSequence())
}

NuclSequence <- function() {
  one_or_more(NuclSequenceString())
}

ProtSequence <- function() {
  one_or_more(ProtSequenceString())
}


## -----------------------------------------------------------------------------
# returns the title after the ">" in the sequence header
parse_header <- function(line) {
  # Study stringr::str_match() to understand what we do here
  m <- stringr::str_match(line, "^>(\\w+)")
  if (is.na(m[1])) {
    return(list()) # signal failure: no title found
  } else {
    return(m[2])
  }
}

# returns a nucleotide sequence string
parse_nucl_sequence_line <- function(line) {
  # The line must consist of GATC from the start (^) until the end ($)
  m <- stringr::str_match(line, "^([GATC]+)$")
  if (is.na(m[1])) {
    return(list()) # signal failure: not a valid nucleotide sequence string
  } else {
    return(m[2])
  }
}

# returns a protein sequence string
parse_prot_sequence_line <- function(line) {
  # The line must consist of ARNDBCEQZGHILKMFPSTWYV from the start (^) until the
  # end ($)
  m <- stringr::str_match(line, "^([ARNDBCEQZGHILKMFPSTWYV]+)$")
  if (is.na(m[1])) {
    return(list()) # signal failure: not a valid protein sequence string
  } else {
    return(m[2])
  }
}

## -----------------------------------------------------------------------------
Header <- function() {
  match_s(parse_header)
}

NuclSequenceString <- function() {
  match_s(parse_nucl_sequence_line)
}

ProtSequenceString <- function() {
  match_s(parse_prot_sequence_line)
}

## -----------------------------------------------------------------------------
Fasta()(fastafile)

## -----------------------------------------------------------------------------
SequenceBlock <- function() {
  MaybeEmpty() %then% 
    Header() %then% 
    (NuclSequence() %or% ProtSequence()) %using%
    function(x) list(x)
}

## -----------------------------------------------------------------------------
Fasta()(fastafile)[["L"]]

## -----------------------------------------------------------------------------
NuclSequence <- function() {
  one_or_more(NuclSequenceString()) %using% 
    function(x) paste(x, collapse="")
}

ProtSequence <- function() {
  one_or_more(ProtSequenceString()) %using% 
  function(x) paste(x, collapse="")
}

## -----------------------------------------------------------------------------
Fasta()(fastafile)[["L"]]

## -----------------------------------------------------------------------------
Header <- function() {
  match_s(parse_header) %using% 
    function(x) list(title = unlist(x))
}

NuclSequence <- function() {
  one_or_more(NuclSequenceString()) %using% 
    function(x) list(type = "Nucl", sequence = paste(x, collapse=""))
}

ProtSequence <- function() {
  one_or_more(ProtSequenceString()) %using% 
    function(x) list(type = "Prot", sequence = paste(x, collapse=""))
}

## -----------------------------------------------------------------------------
d <- Fasta()(fastafile)[["L"]]
d

## -----------------------------------------------------------------------------
invisible(lapply(d, function(x) {cat(x$type, x$title, x$sequence, "\n")}))

## ----echo=FALSE---------------------------------------------------------------
qtemp <- c(
  "#### INTRO",
  "## Title about a set of questions",
  "",
  "This is optional introductory text to a set of questions.",
  "Titles preceded by four hashes are not allowed in a question template.",
  "",
  "#### QUESTION",
  "This is the first question",
  "",
  "#### TIP",
  "This would be a tip. tips are optional, and multiple tips can be given. Tips are",
  "wrapped in hide-reveal style html elements.",
  "",
  "#### TIP",
  "This would be a second tip.",
  "",
  "#### ANSWER",
  "The answer to the question is optional and is wrapped in a hide-reveal html element.",
  "",
  "#### QUESTION",
  "This is the second question. No tips for this one",
  "",
  "#### ANSWER",
  "Answer to the second question"
)

## ----echo=FALSE, comment=NA---------------------------------------------------
cat(paste0(c(qtemp,"","<optionally more questions>"), collapse="\n"))

## -----------------------------------------------------------------------------
HeaderAndContent <- function(type) {
    (Header(type) %then% Content()) %using% 
    function(x) list(list(type=type, content=unlist(x)))
}

## -----------------------------------------------------------------------------
Intro <- function() HeaderAndContent("intro")
Question <- function() HeaderAndContent("question")
Tip <- function() HeaderAndContent("tip")
Answer <- function() HeaderAndContent("answer")

## -----------------------------------------------------------------------------
Header <- function(type) satisfy(header(type)) %ret% NULL

# This must also be a generic function: a function that generates a function to 
# recognize a header of type 'type'
header <- function(type) {
  function(x) grepl(paste0("^####\\s+", toupper(type), "\\s*"), x)
}

## -----------------------------------------------------------------------------
Content <- function() {
  (one_or_more(match_s(content))) %using%
    function(x) stringr::str_trim(paste0(x,collapse="\n"), "right")
}

content <- function(x) {
  if (grepl("^####", x)) list()
  else x
}

## -----------------------------------------------------------------------------
Template <- function() {
  zero_or_more(Intro()) %then%
    one_or_more(QuestionBlock()) %then%
    eof()
}

## -----------------------------------------------------------------------------
QuestionBlock <- function() {
    Question() %then%
    zero_or_more(Tip()) %then%
    zero_or_one(Answer()) %using%
    function(x) list(x)
}

## -----------------------------------------------------------------------------
reporter(Template())(qtemp)

