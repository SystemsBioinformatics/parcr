
#' Applying a parser to a split string
#'
#' @param p A parser.
#' @param split A single element character vector  (or object which can be
#'  coerced to such) containing [regular expression](base::regex)(s)
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
#' @export
#'
# @examples
split_by <- function(p, split, finish=TRUE, fixed=FALSE, perl=FALSE) {
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
