<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!--[![R-CMD-check](https://github.com/SystemsBioinformatics/parcr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/SystemsBioinformatics/parcr/actions/workflows/R-CMD-check.yaml) -->
<!-- badges: end -->

## Construct parser combinator functions for parsing character vectors

This R package contains tools to construct parser combinator functions, higher 
order functions that parse input. The main application of parsers constructed 
with this package is the parsing of text files, read into a character vector. 
This package was inspired by the package 
["Ramble"](https://github.com/chappers/Ramble) by Chapman Siu and by the paper
["Higher-order functions for parsing"](https://doi.org/10.1017/S0956796800000411) 
by [Graham Hutton](https://orcid.org/0000-0001-9584-5150) (1992).
