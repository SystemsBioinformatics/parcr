## Derived, "higher order" parsers start with a capital

#' @title Recognize empty lines
#'
#' @description
#'
#' An empty line is a line that consists entirely of space-like characters.
#' `EmptyLine` is a parser that recognizes one empty line, `Spacer` recognizes
#' one or more empty lines and `MaybeEmpty` recognizes zero or more empty
#' lines. `EmptyLine` returns the empty line but `Spacer` and `MaybeEmpty`
#' discard these.
#'
#' @section Pseudocode:
#'
#' \preformatted{
#' space_like_eraser(x):
#'     d = replace all "\\\\s+" in x by ""
#'     if d=="" TRUE else FALSE
#'
#' Emptyline: satisfy(space_like_eraser)
#'
#' Spacer: one_or_more(EmptyLine()) \%ret\% null
#'
#' MaybeEmpty: zero_or_more(EmptyLine()) \%ret\% null
#' }
#'
#' @importFrom stringr str_replace_all
#' @export
#' @examples
#' EmptyLine() (" \t  ") # success
#' EmptyLine() ("    .") # failure
#' EmptyLine() ("") # success
EmptyLine <- function() {
  satisfy(function(x) {stringr::str_replace_all(x, "\\s+", "") == ""})
}

#' @rdname EmptyLine
#' @export
#' @examples
#' Spacer() (c("   \t  ", "    ", "abc"))
#' Spacer() (c("            ", "    ", "Important text"))
#' Spacer() (c("Important text")) # failure, missing empty line
Spacer <- function() {
  one_or_more(EmptyLine()) %ret% NULL
}

#' @rdname EmptyLine
#' @export
#' @examples
#' MaybeEmpty() (c("            ", "    ", "Important text")) # success, just as Spacer()
#' MaybeEmpty() (c("Important text")) # success, in contrast to Spacer()
MaybeEmpty <- function() {
  (zero_or_more(EmptyLine())) %ret% NULL
}

#' #' Extracts all integer and floating point numbers from a line
#' #'
#' #' Ignores any other symbols. It tests whether exactly n numbers are found.
#' #'
#' #' @param n An integer.
#' #'
#' #' @return A parser.
#' #' @export
#' #' @examples
#' #' Numbers(3) ('1  2  3') #success
#' #' Numbers(3) ('1101\t201.5\t33') # success
#' #' Numbers(2) ('1101\t201.5\t33') # failure
#' #' Numbers(4) ('1101\t201.5\t33') # failure
#' #'
# Numbers <- function(n) {
#   (satisfy( function(x) {
#     (stringr::str_extract_all(x, pattern = "[\\d\\.]+", simplify = TRUE) |>
#        as.vector() |> length()) == n
#   })) %using%
#     function(x) {
#       stringr::str_extract_all(x, pattern = "[\\d\\.]+", simplify = TRUE) |>
#         as.vector() |> as.numeric()
#     }
# }
