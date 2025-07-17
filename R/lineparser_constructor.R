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
#' the pattern contains a captured group then that group will be returned upon
#' matching. If there are multiple capture groups only the first will be
#' returned
#'
#' @examples
#' parse_header <- lineparser("^>(\\w+)")
#' parse_header(">correct_header")
#' parse_header(">incorrect header")
#'
lineparser <- function(match_pattern) {
  function(line) {
    m <- stringr::str_match(line, match_pattern)
    if (is.na(m[1])) {
      return(list()) # signal failure
    } else {
      if (length(m) > 1) {
        return(m[2])
      }
    }
  }
}
