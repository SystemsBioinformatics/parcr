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

#' Ignore all until the end
#'
#' Sometimes, a parser has found the end of the text that needs to be parsed and
#' all following lines can be ignored. `Ignore` will read and discard all
#' following lines until the end of the file, empty or not. So, this parser will
#' consume all elements until the end of the input vector.
#'
#' @details
#' This parser is identical to
#'
#' \preformatted{
#' zero_or_more(satisfy(function(x) TRUE)) %ret% NULL
#' }
#'
#' @inherit satisfy return
#' @export
#' @examples
#'
#' starts_with_a <- function(x) grepl("^a",x)
#' p <- function() {
#'   one_or_more(satisfy(starts_with_a)) %then%
#'   (literal("~End") %ret% NULL) %then%
#'   Ignore() %then%
#'   eof()
#' }
#' p()(c("ab","abc","~End","boring stuff","more stuff"))
Ignore <- function() {
  zero_or_more(satisfy(function(x) TRUE)) %ret% NULL
}
