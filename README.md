
<!-- README.md is generated from README.Rmd. Edit README.Rmd -->
<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/parcr)](https://cran.r-project.org/package=parcr)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/SystemsBioinformatics/parcr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/SystemsBioinformatics/parcr/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

## Construct parser combinator functions for parsing character vectors

This R package contains tools to construct parser combinator functions,
higher order functions that parse input. The main goal of this package
is to simplify the creation of *transparent* parsers for structured text
files generated by machines like laboratory instruments. Such files
consist of lines of text organized in higher-order structures like
headers with metadata and blocks of measured values. To read these data
into R you first need to create a parser that processes these files and
creates R-objects as output. The `parcr` package simplifies the task of
creating such parsers.

This package was inspired by the package
[“Ramble”](https://github.com/8bit-pixies/Ramble) by Chapman Siu and
co-workers and by the paper [“Higher-order functions for
parsing”](https://doi.org/10.1017/S0956796800000411) by [Graham
Hutton](https://orcid.org/0000-0001-9584-5150) (1992).

## Installation

Install the stable version from CRAN

    install.packages("parcr")

To install the development version including its vignette run the
following command

    devtools::install_github("SystemsBioinformatics/parcr", build_vignettes=TRUE)

## Example application: a parser for *fasta* sequence files

As an example of a realistic application we write a parser for
fasta-formatted files for nucleotide and protein sequences. We use a few
simplifying assumptions about this format for the sake of the example.
Real fasta files are more complex than we pretend here.

*Please note that more background about the functions that we use here
is available in the package documentation. Here we only present a
summary.*

A fasta file with mixed sequence types could look like the example
below:

    >sequence_A
    GGTAAGTCCTCTAGTACAAACACCCCCAAT
    TCTGTTGCCAGAAAAAACACTTTTAGGCTA
    >sequence_B
    ATTGTGATATAATTAAAATTATATTCATAT
    TATTAGAGCCATCTTCTTTGAAGCGTTGTC
    TATGCATCGATC

    >sequence_C
    MTEITAAMVKELRESTGAGMMDCKNALSET
    NGDFDKAVQLLREKGLGKAAKKADRLAAEG
    ENEYKALVAELEKE

Since fasta files are text files we could read such a file using
`readLines()` into a character vector. The package provides the data set
`fastafile` which contains that character vector.

``` r
data("fastafile")
```

We can distinguish the following higher order components in a fasta
file:

- A **fasta** file: consists of one or more **sequence blocks** until
  the **end of the file**.
- A **sequence block**: consist of a **header** and a **nucleotide
  sequence** or a **protein sequence**. A sequence block could be
  preceded by zero or more **empty lines**.
- A **nucleotide sequence**: consists of one or more **nucleotide
  sequence strings**.
- A **protein sequence**: consists of one or more **protein sequence
  strings**.
- A **header** is a *string* that starts with a “\>” immediately
  followed by a **title** without spaces.
- A **nucleotide sequence string** is a *string* without spaces that
  consists *entirely* of symbols from the set `{G,A,T,C}`.
- A **protein sequence string** is a *string* without spaces that
  consists *entirely* of symbols from the set
  `{A,R,N,D,B,C,E,Q,Z,G,H,I,L,K,M,F,P,S,T,W,Y,V}`.

It now becomes clear what we mean when we say that the package allows us
to write *transparent* parsers: the description above of the structure
of fasta files can be put straight into code for a `Fasta()` parser:

``` r
Fasta <- function() {
  one_or_more(SequenceBlock()) %then%
    eof()
}

SequenceBlock <- function() {
  MaybeEmpty() %then% 
    Header() %then% 
    (NuclSequence() %or% ProtSequence()) %using%
    function(x) list(x)
}

NuclSequence <- function() {
  one_or_more(NuclSequenceString()) %using% 
    function(x) list(type = "Nucl", sequence = paste(x, collapse=""))
}

ProtSequence <- function() {
  one_or_more(ProtSequenceString()) %using% 
    function(x) list(type = "Prot", sequence = paste(x, collapse=""))
}
```

Functions like `one_or_more()`, `%then%`, `%or%`, `%using%`, `eof()` and
`MaybeEmpty()` are defined in the package and are the basic parsers with
which the package user can build complex parsers. The `%using%` operator
uses the function on its right-hand side to modify parser output on its
left hand side. Please see the vignette in the `parcr` package for more
explanation why this is useful or necessary even.

Notice that the new parser functions that we define above are higher
order functions taking no input, hence the empty argument brackets `()`
behind their names. Now we need to define the line-parsers `Header()`,
`NuclSequenceString()` and `ProtSequenceString()` that recognize and
process the header line and single lines of nucleotide or protein
sequences in the character vector `fastafile`. We use functions from
`stringr` to do this in a few helper functions, and we use `match_s()`
to to create `parcr` parsers from these.

``` r
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
```

Then we define the line-parsers.

``` r
Header <- function() {
  match_s(parse_header) %using% 
    function(x) list(title = unlist(x))
}

NuclSequenceString <- function() {
  match_s(parse_nucl_sequence_line)
}

ProtSequenceString <- function() {
  match_s(parse_prot_sequence_line)
}
```

where `match_s()` is also a parser defined in `parcr`.

Now we have all the elements that we need to apply the `Fasta()` parser.

``` r
Fasta()(fastafile)
#> $L
#> $L[[1]]
#> $L[[1]]$title
#> [1] "sequence_A"
#> 
#> $L[[1]]$type
#> [1] "Nucl"
#> 
#> $L[[1]]$sequence
#> [1] "GGTAAGTCCTCTAGTACAAACACCCCCAATTCTGTTGCCAGAAAAAACACTTTTAGGCTA"
#> 
#> 
#> $L[[2]]
#> $L[[2]]$title
#> [1] "sequence_B"
#> 
#> $L[[2]]$type
#> [1] "Nucl"
#> 
#> $L[[2]]$sequence
#> [1] "ATTGTGATATAATTAAAATTATATTCATATTATTAGAGCCATCTTCTTTGAAGCGTTGTCTATGCATCGATC"
#> 
#> 
#> $L[[3]]
#> $L[[3]]$title
#> [1] "sequence_C"
#> 
#> $L[[3]]$type
#> [1] "Prot"
#> 
#> $L[[3]]$sequence
#> [1] "MTEITAAMVKELRESTGAGMMDCKNALSETNGDFDKAVQLLREKGLGKAAKKADRLAAEGENEYKALVAELEKE"
#> 
#> 
#> 
#> $R
#> list()
```

The output of the parser consists of two elements, `L` and `R`, where
`L` contains the parsed and processed part of the input and `R` the
remaining un-parsed part of the input. Since we explicitly demanded to
parse until the end of the file by the `eof()` function in the
definition of the `Fasta()` parser, the `R` element contains an empty
list to signal that the parser was indeed at the end of the input.
Please see the package documentation for more examples and explanation.

Finally, let’s present the result of the parse more concisely using the
names of the elements inside the `L` element:

``` r
d <- Fasta()(fastafile)[["L"]]
invisible(lapply(d, function(x) {cat(x$type, x$title, x$sequence, "\n")}))
#> Nucl sequence_A GGTAAGTCCTCTAGTACAAACACCCCCAATTCTGTTGCCAGAAAAAACACTTTTAGGCTA 
#> Nucl sequence_B ATTGTGATATAATTAAAATTATATTCATATTATTAGAGCCATCTTCTTTGAAGCGTTGTCTATGCATCGATC 
#> Prot sequence_C MTEITAAMVKELRESTGAGMMDCKNALSETNGDFDKAVQLLREKGLGKAAKKADRLAAEGENEYKALVAELEKE
```
