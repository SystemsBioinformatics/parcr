
#' Applying a parser to a split string
#'
#' @description
#' Splits a string by using a split pattern and then applies the parser `p`
#' to the resulting character vector. If `finish = TRUE` then the parser should
#' completely consume its input, otherwise the parser fails. If
#' `finish = FALSE` then any remaining part of the string is discarded.
#'
#' @param p A parser.
#' @param split A **single element** character vector (or object which can be
#'  coerced to such) containing \link{regular expression}(s)
#'  (unless fixed = TRUE) to use for splitting. If empty matches occur, in
#'  particular if split has length 0, x is split into single characters.
#' @param finish logical. Should the parser should completely consume the
#'  string? Defaults to `TRUE`.
#' @inheritParams base::strsplit
#'
#'
#' @details
#' The function [base::strsplit()] is used to perform the splitting. The
#' parameters `split`, `fixed` and `perl` are inherited from that function.
#'
#'
#' @return A parser.
#' @seealso [base::strsplit()], [by_symbol()]
#' @export
#'
#' @examples
#' by_split((literal("a") %then% literal("b")),"\\t") ("a\tb") # success
#' by_split((literal("a") %then% literal("b")),"\\t") ("a\tb\tc") # failure
#' by_split((literal("a") %then% literal("b")),"\\t", finish=FALSE) ("a\tb\tc") # success
by_split <- function(p, split, finish=TRUE, fixed=FALSE, perl=FALSE) {
  # if (!(length(split)==1)) stop("Argument 'split' should be a vector of length 1")
  function(x) {
    if (is.empty(x)) fail()(x)
    else {
      w <- unlist(strsplit(x[1], split, fixed, perl, useBytes = FALSE))
      r <- x[-1]
      l1 <- p(w)
      if (failed(l1)) fail()(x)
      else {
        if (finish) {
          if (!is.empty(l1$R)) (fail()(x))
          else succeed(l1$L)(r)
        } else {
          succeed(l1$L)(r)
        }
      }
    }
  }
}

#' Applying a parser to individual symbols of a string
#'
#' @inheritParams by_split
#'
#' @description
#' Splits a string to individual symbols and then applies the parser `p` to the
#' resulting character vector, otherwise the parser fails. If `finish = TRUE`
#' then the parser should completely consume its input. If `finish = FALSE`
#' then any remaining part of the string is discarded.
#'
#' This function is identical to `by_split(p, "", finish)`.
#'
#' @return A parser.
#' @seealso [by_split()]
#' @export
#'
#' @examples
#' by_symbol(exactly(3,literal("a"))) (c("aaa", "bb")) # success
#' by_symbol(exactly(3,literal("a"))) (c("aaaa", "bb")) # failure
by_symbol <- function(p, finish=TRUE) {
  by_split(p, "", finish)
}
