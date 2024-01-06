
#' Turn a parser into an error messaging parser
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
    if (!failed(r)) r else parser_error2(marker_val(r))
  }
}

parser_error2 <- function (nr) {
  stop("Parser failed on line ", nr, " of input")
}
