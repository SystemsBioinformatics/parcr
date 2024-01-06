Parser <- function(p) {
  reset_LNR()
  function(x) {
    r <- p(x)
    if (!failed(r)) r else parser_error2(tracker_val(r))
  }
}

parser_error2 <- function (nr) {
  stop("Parser failed on line ", nr, " of input")
}
