## Derived, "higher order" parsers start with a capital

#' Recognize empty lines
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
#'     d = x in which all "\\\\s+" are replaced by ""
#'     if d=="" TRUE else FALSE
#'
#' Emptyline: satisfy(space_like_eraser)
#'
#' Spacer: one_or_more(EmptyLine()) \%ret\% null
#'
#' MaybeEmpty: zero_or_more(EmptyLine()) \%ret\% null
#' }
#'
#' where `null` is the empty vector.
#'
#' @inherit satisfy return
#' @export
#' @examples
#' EmptyLine() (" \t  ") # success
#' EmptyLine() ("    .") # failure
#' EmptyLine() ("") # success
EmptyLine <- function() {
  satisfy(function(x) {gsub("\\s+", "", x) == ""})
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

