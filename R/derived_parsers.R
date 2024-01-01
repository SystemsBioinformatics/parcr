## Derived, "higher order" parsers start with a capital

#' @title Recognize empty lines
#'
#' @description
#'
#' An empty line is a line that consists entirely of space-like characters.
#' `Empty.line` is a parser that recognizes one empty line and `Spacer`
#' recognizes one or more empty lines and `MaybeEmpty` recognizes zero or more
#' empty lines. `Empty.line` actually returns the empty line but `Spacer` and
#' `MaybeEmpty` discard the empty lines.
#'
#' @section Definitions:
#'
#' `Empty.line <- function() {`
#'
#' `satisfy(function(x) {stringr::str_replace_all(x, "\\s+", "") == ""})`
#'
#' `}`
#'
#' `Spacer <- function() {one.or.more(Empty.line()) %ret% NULL}`
#'
#' `MaybeEmpty <- function() {(zero.or.more(Empty.line())) %ret% NULL}`
#'
#' @importFrom stringr str_replace_all
#' @export
#' @examples
#' Empty.line() (c(' \t  ')) # success
#' Empty.line() (c('    .')) # failure
Empty.line <- function() {
  satisfy(function(x) {stringr::str_replace_all(x, "\\s+", "") == ""})
}

#' @rdname Empty.line
#' @export
#' @examples
#' Spacer() (c("   \t  ", "    ", "abc"))
#' Spacer() (c("            ", "    ", "Important text"))
#' Spacer() (c("Important text")) # failure
Spacer <- function() {
  one.or.more(Empty.line()) %ret% NULL
}

#' @rdname Empty.line
#' @export
#' @examples
#' MaybeEmpty() (c("            ", "    ", "Important text"))
#' MaybeEmpty() (c("Important text")) # success, in contrast to Spacer()
MaybeEmpty <- function() {
  (zero.or.more(Empty.line())) %ret% NULL
}

#' Extracts all integer and floating point numbers from a line
#'
#' Ignores any other symbols. It tests whether exactly n numbers are found.
#'
#' @param n An integer.
#'
#' @return A parser.
#' @export
#' @examples
#' Numbers(3) ('1  2  3')
#' Numbers(3) ('1101\t201\t33')
#'
Numbers <- function(n) {
  (satisfy( function(x) {
    (stringr::str_extract_all(x, pattern = "[\\d\\.]+", simplify = TRUE) |>
       as.vector() |> length()) == n
  })) %using%
    function(x) {
      stringr::str_extract_all(x, pattern = "[\\d\\.]+", simplify = TRUE) |>
        as.vector() |> as.numeric()
    }
}
