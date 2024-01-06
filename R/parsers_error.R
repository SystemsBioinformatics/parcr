nofail <- function(p) {
  function (x) {
    if (failed(p(x))) parser_error(x)
    else p(x)
  }
}

parser_error <- function (x) {
  stop("Parser failed in line:\n", x[1])
}

# Parser <- function() {
#   linenr <- 0
#   function(p) {
#     function(x) {
#       r <- p(x)
#       if (!failed(r)) r else parser_error2(linenr)
#     }
#   }
# }

Parser <- function() {
  linenr <- 0
  function(x) {
    (satisfy(starts_with_a) %then% satisfy(starts_with_b))(x)
  }
}

parser_error2 <- function (nr) {
  stop("Parser failed in line ", nr)
}
