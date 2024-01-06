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

# Fail should produce a tracker
fail <- function(lnr=LNR()) {
  function(x) new_tracker(lnr)
}

# The or() function for error tracking
# need to keep track of how many lines are parsed until failure in each branch
`%or%` <- function(p1, p2) {
  function(x) {
    init_lnr <- LNR()
    r1 <- p1(x)
    if (!failed(r1)) r1 else {
      set_LNR(init_lnr) # reset to where started
      r2 <- p2(x)
      if (!failed(r2)) r2 else {
        return(new_tracker(max(tracker_val(r1), tracker_val(r2)))) # perhaps new method for max?
      }
    }
  }
}

# The then() function for error tracking
# need to keep track of how many lines are parsed. Every successful then we
# move one line further
`%then%` <- function(p1, p2) {
  function(x) {
    r1 <- p1(x)
    if (failed(r1)) r1
    else {
      inc_LNR()
      r2 <- p2(r1$R)
      if (failed(r2)) r2 else succeed(c(r1$L, r2$L))(r2$R)
    }
  }
}

`%xthen%` <- function(p1, p2) {
  function(x) {
    r1 <- p1(x)
    if (failed(r1)) r1
    else {
      inc_LNR()
      r2 <- p2(r1$R)
      if (failed(r2)) r2 else succeed(r1$L) (r2$R)
    }
  }
}

`%thenx%` <- function(p1, p2) {
  function(x) {
    r1 <- p1(x)
    if (failed(r1)) r1
    else {
      inc_LNR()
      r2 <- p2(r1$R)
      if (failed(r2)) r2 else succeed(r2$L) (r2$R)
    }
  }
}

# succeed(character(0)) does not shift one element to the right.
zero_or_more <- function(p) {
  (p %then% zero_or_more(p)) %or% (succeed(character(0)) %using% function(x){dec_LNR(); x})
}