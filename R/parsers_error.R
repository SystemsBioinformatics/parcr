#' Turn a parser into an error messaging parser.
#'
#' @param p a parser.
#'
#' @return A parser result or an error message about the line where the parser
#'         failed.
#' @export
#'
#' @examples
#' # Yields an error message about parser failing on line 5
#' \dontrun{
#' Parser(match_n(3,literal("a") %then% literal("t")) %then% eof())(c(rep(c("a","t"),2),"t","t"))
#' }
#' Parser(match_n(2,literal("a") %then% literal("t")) %then% eof())(rep(c("a","t"),2)) # success
Parser <- function(p) {
  reset_LNR()
  function(x) {
    r <- p(x)
    if (!failed(r)) {
      if (!finished(r)) {
        # message that we did not completely consume the input and that user should consider using eof()
      }
      r
    } else parser_error(nr=marker_val(r), content=x[marker_val(r)])
  }
}

# from https://adv-r.hadley.nz/conditions.html
stop_custom <- function(.subclass, message, call = NULL, ...) {
  err <- structure(
    list(
      message = message,
      call = call,
      ...
    ),
    class = c(.subclass, "error", "condition")
  )
  stop(err)
}

parser_error <- function(nr, content) {
  message = paste0("Parser failed on line ", nr, " of input.\nLine content: \"",content,"\"")
  stop_custom (.subclass = "error_parser",
               message = message,
               linenr = nr,
               linecontent = content)
}
