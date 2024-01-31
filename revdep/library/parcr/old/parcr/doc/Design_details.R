## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, echo=FALSE--------------------------------------------------------
library(parcr)

## ----out.width="30%", fig.align='left', echo=FALSE, fig.cap="<i>**Illustration of marker positioning when a parser fails on an input vector of length 6**. Each filled dot represents a successful parse. A `%then%` combinator shifts to the next element whereas an `%or%` combinator applies alternative parsers to the input. The parser fails at the red crosses and the red square. A marker (red square) will be put at the parser that fails at the input element with the highest index, which is element #5 here.</i>"----
knitr::include_graphics("tree.png")

## -----------------------------------------------------------------------------
p <- function() {
  literal("A") %then%
    literal("B") %then%
    (arm1() %or% arm2())
}

arm1 <- function() {
  literal("D") %then%
    literal("E") %then%
    literal("F") %then%
    literal("G")
}

arm2 <- function() {
  literal("C") %then% 
    (arm21() %or% arm22())
}

arm21 <- function() {
  literal("D") %then%
    literal("F") %then%
    literal("G")
}

arm22 <- function() {
  literal("E") %then%
    literal("F") %then%
    literal("G")
}

try(reporter((p() %then% eof()))(LETTERS[1:6]))

## -----------------------------------------------------------------------------
try(reporter((p() %then% eof()))(c("A","B","C","E","F","G")))

