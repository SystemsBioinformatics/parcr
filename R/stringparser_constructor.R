#' Helper function emulating `stringr::str_match()`
#'
#' @noRd
str_match <- function(line, match_pattern, ...) {
  regmatches(line, regexec(match_pattern, line, ...))[[1]]
}

#' String parser constructor
#'
#' @description
#' Produce a string parser based on [stringr::str_match()], to be used with
#' [match_s()].
#'
# @usage parse_header = stringparser(match_pattern = "^>(\\w+)", reshape = function(x) {x})
#'
#' @details
#' This function uses [regexec()] to produce a string parser. Parsers
#' created with this constructor return the failure signal `list()` when a
#' string does not match the `match_pattern`. If the pattern contains captured
#' groups then these groups will be returned as a character vector upon matching.
#' If there is no capture group then the function will return silently upon
#' matching the pattern. You can provide a function to the `reshape` argument
#' to change the output upon matching.
#'
#' You always have to wrap the parsers made with this constructor in
#' `match_s()` to create a parser combinator. I decided not to include this
#' standard pattern in the `stringparser` constructor itself because it
#' complicates testing of these parsers.
#'
#' @param match_pattern A regular expression that matches the string.
#'
#' @param reshape A function that takes the character vector of captured strings
#' and modifies it to a desired output. By default this is the [identity()]
#' function.
#'
#' @param ... additional parameters passed on to [regexec()]
#'
#' @seealso [match_s()], [regexec()]
#'
#' @export
#'
#' @examples
#' # single capture group
#' parse_header <- stringparser("^>(\\w+)")
#' parse_header(">correct_header") # returns "correct_header"
#' parse_header("> incorrect_header") # returns list()
#'
#' # multiple capture groups
#' parse_keyvalue <- stringparser("(\\w+):\\s?(\\w+)")
#' parse_keyvalue("key1: value1") # returns c("key1", "value1")
#'
#' # modify output
#' parse_keyvalue_df <- stringparser(
#'   "(\\w+):\\s?(\\w+)",
#'   function(x) data.frame(key = x[1], value = x[2])
#' )
#' parse_keyvalue_df("key1: value1") # returns a data frame
#'
stringparser <- function(match_pattern, reshape = identity, ...) {
  function(line) {
    m <- str_match(line, match_pattern, ...)
    if (length(m) == 0) {
      return(list()) # signal failure
    } else {
      m <- m[-1] # Remove full match, keep capture groups
      return(reshape(m))
    }
  }
}
