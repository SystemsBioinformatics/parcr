#' String parser constructor
#'
#' @description
#' Produce a string parser based on stringr::str_match
#'
#' @details
#' This function uses stringr::str_match to produce a string parser. It returns
#' the failure signal `list()` when a string does not match the `match_pattern`
#'
#' @param match_pattern A regular expression that matches the entire string. If
#' the pattern contains captured groups then these groups will be returned upon
#' matching. If there is no capture group then the function will return silently
#' upon matching the pattern.
#'
#' @param return A character vector of length 1 that yields an expression when
#' parsed that defines the output. The captured groups are available as the
#' variable `m` which is a character vector of length equal to the number of
#' captured groups. By default, this character vector will be returned.
#' 
#' @export
#'
#' @examples
#' # single capture group
#' parse_header <- stringparser("^>(\\w+)")
#' parse_header(">correct_header")     # returns "correct_header"
#' parse_header("> incorrect_header")  # returns list()
#'
#' # multiple capture groups
#' parse_keyvalue <- stringparser("(\\w+):\\s?(\\w+)")
#' parse_keyvalue("key1: value1")      # returns c("key1", "value1")
#'
#' # arbitrary output
#' parse_keyvalue_df <- stringparser("(\\w+):\\s?(\\w+)", "data.frame(key = m[1], value = m[2])")
#' parse_keyvalue_df("key1: value1")      # returns a data frame
#'
stringparser <- function(match_pattern, return = "m") {
  function(line) {
    m <- stringr::str_match(line, match_pattern)
    if (is.na(m[1])) {
      return(list()) # signal failure
    } else {
      m <- m[-1]
      if (length(m) > 0) {
        return(eval(parse(text = return)))
      }
    }
  }
}
