#' Line parser constructor
#'
#' @description
#' Produce a line parser based on stringr::str_match
#'
#' @details
#' This function uses stringr::str_match to produce a line parser. It returns
#' the failure signal `list()` when a line does not match the `match_pattern`
#'
#' @param match_pattern A regular expression that matches the entire line. If
#' the pattern contains captured groups then these groups will be returned upon
#' matching. If there is no capture group then the function will return silently
#' upon matching the pattern.
#'
#' @param return A character vector of length 1 that yields an expression when
#' parsed that defines the output. The captured groups are availble as the
#' character vector `m`. By default, this character vector will be returned.
#'
#' @examples
#' parse_header <- lineparser("^>(\\w+)")
#' parse_header(">correct_header")     # returns "correct_header"
#' parse_header("> incorrect_header")  # returns list()
#' @export
lineparser <- function(match_pattern, return = "m") {
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
