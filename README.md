<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/SystemsBioinformatics/parcr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/SystemsBioinformatics/parcr/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

## Construct parser combinator functions for parsing character vectors

This R package contains tools to construct parser combinator functions, higher 
order functions that parse input. The main goal of this package is to simplify
the creation of transparent parsers for structured text files generated by 
machines like laboratory instruments. Such files consist of lines of text 
organized in higher-order structures like headers with metadata and blocks of 
measured values. To read these data into R you first need to create a parser
that processes these files and creates R-objects as output. The `parcr` package
simplifies the task of creating such parsers.

This package was inspired by the package 
["Ramble"](https://github.com/chappers/Ramble) by Chapman Siu and co-workers 
and by the paper
["Higher-order functions for parsing"](https://doi.org/10.1017/S0956796800000411) 
by [Graham Hutton](https://orcid.org/0000-0001-9584-5150) (1992).

## Installation

To install the package including its vignette run the following command

```
install_github("https://github.com/SystemsBioinformatics/parcr/releases/latest", build_vignettes=TRUE)
```

## Example application: a parser for *fasta* sequence files

As an example of a realistic application we write a parser for 
fasta-formatted files for nucleotide sequences. We use a few simplifying 
assumptions about this format for the sake of the example. Real fasta files are
more complex than we pretend here.

*Please note that more background about the functions that we use here is 
available in the package documentation. Here we only present a summary.*

A nucleotide fasta file could look like the example below

```
>sequence_A
GGTAAGTCCTCTAGTACAAACACCCCCAAT
TCTGTTGCCAGAAAAAACACTTTTAGGCTA

>sequence_B
ATTGTGATATAATTAAAATTATATTCATAT
TATTAGAGCCATCTTCTTTGAAGCGTTGTC
TATGCATCGATCGACGACTG
```

Since fasta files are text files we could read such a file using `readLines()`
into a character vector. The package provides the data set `fastafile` to
which is that character vector.

```r
data("fastafile")
```

We can distinguish the following higher order components in a fasta file:
 
- A **fasta** file: consists of one or more **sequence blocks** until the 
  **end of the file**.
- A **sequence block**: consist of a **header** and a **sequence**. A 
  sequence block could be preceded by zero or more **empty lines**.
- A **sequence**: consists of one or more **sequence strings**.
- A **header** is a *string* that starts with a ">" immediately followed by
  a **title** without spaces.
- A **sequence string** is a *string* without spaces that consists *entirely* 
  of symbols from the set {`G`,`A`,`T`,`C`}.

It now becomes clear what we mean by the assertion that the package allows us
to write transparent parsers: the description above of the structure of fasta
files can be put straight into code for a `Fasta()` parser:

```r
Fasta <- function() {
  one_or_more(SequenceBlock()) %then%
    eof()
}

SequenceBlock <- function() {
  MaybeEmpty() %then% 
    Header() %then% 
    Sequence() %using%
    function(x) list(x)
}

Sequence <- function() {
  one_or_more(SequenceString()) %using% 
    function(x) list(sequence = paste(x, collapse=""))
}
```

Functions like `one_or_more()`, `%then%`, `%using%`, `eof()` and
`MaybeEmpty()` are defined in the package and are the basic parsers with
which the package user can build complex parsers. The `%using%` operator uses
the function on its right-hand side to modify parser output on its left hand 
side. Please see the vignette in the `parcr` package for more explanation why
this is useful.

Notice that the new parser functions that we define above are higher order 
functions taking no input, hence the empty argument brackets `()` behind their
names. Now we need to define the line-parsers `Header()` and `SequenceString()`
that recognize and process the header line and single lines of nucleotide 
sequences in the character vector `fastafile`. We use functions from
`stringr` to do this in a few helper functions, and we use `match_s()` to shape
these parsers.

```r
# returns the title after the ">" in the sequence header
parse_header <- function(line) {
  # Study stringr::str_match() to understand what we do here
  m <- stringr::str_match(line, "^>(\\w+)")
  if (is.na(m[1])) {
    return(list()) # signal parser failure: no title found
  } else {
    return(m[2])
  }
}

# returns a sequence string
parse_sequence_line <- function(line) {
  # The line must consist of GATC from the start (^) until the end ($)
  m <- stringr::str_match(line, "^([GATC]+)$")
  if (is.na(m[1])) {
    return(list()) # signal parser failure: not a valid nucleotide sequence string
  } else {
    return(m[2])
  }
}
```

Then we define the line-parsers.

```r
Header <- function() {
  match_s(parse_header) %using% 
    function(x) list(title = unlist(x))
}

SequenceString <- function() {
  match_s(parse_sequence_line)
}
```
where `match_s()` is also a parser defined in `parcr`.

Now we have all the elements that we need to apply the `Fasta()` parser.

```r
Fasta()(fastafile)
```

**output:**

```
$L
$L[[1]]
$L[[1]]$title
[1] "sequence_A"

$L[[1]]$sequence
[1] "GGTAAGTCCTCTAGTACAAACACCCCCAATTCTGTTGCCAGAAAAAACACTTTTAGGCTA"


$L[[2]]
$L[[2]]$title
[1] "sequence_B"

$L[[2]]$sequence
[1] "ATTGTGATATAATTAAAATTATATTCATATTATTAGAGCCATCTTCTTTGAAGCGTTGTCTATGCATCGATCGACGACTG"



$R
list()
```

Let's present the result more concisely using the names of these elements:

```r
d <- Fasta()(fastafile)[["L"]]
invisible(lapply(d, function(x) {cat(x$title, x$sequence, "\n")}))
```

```
sequence_A GGTAAGTCCTCTAGTACAAACACCCCCAATTCTGTTGCCAGAAAAAACACTTTTAGGCTA 
sequence_B ATTGTGATATAATTAAAATTATATTCATATTATTAGAGCCATCTTCTTTGAAGCGTTGTCTATGCATCGATCGACGACTG 
```
