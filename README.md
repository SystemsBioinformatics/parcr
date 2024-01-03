<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/SystemsBioinformatics/parcr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/SystemsBioinformatics/parcr/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

## Construct parser combinator functions for parsing character vectors

This R package contains tools to construct parser combinator functions, higher 
order functions that parse input. The main goal of this package is to simplify
the creation of parsers for structured text files generated by machines like
laboratory instruments. Such files consist of lines of text organized in 
higher-order structures like headers with metadata and blocks of measured 
values. To read these data into R you first need to create a parser that 
processes these files and creates R-objects as output. The `parcr` package
simplifies the task of creating such parsers.

This package was inspired by the package 
["Ramble"](https://github.com/chappers/Ramble) by Chapman Siu and co-workers 
and by the paper
["Higher-order functions for parsing"](https://doi.org/10.1017/S0956796800000411) 
by [Graham Hutton](https://orcid.org/0000-0001-9584-5150) (1992).

## Installation

Top install the package including its vignette run the following command

```
install_github("https://github.com/SystemsBioinformatics/parcr/releases/tag/v0.2.5", build_vignettes=TRUE)
```
