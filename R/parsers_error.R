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

# The or() function for error tracking
# need to keep track of how many lines are parsed until failure in each branch
`%orE%` <- function(p1, p2) {
  function(x) {
    init_lnr <- LNR()
    r1 <- p1(x)
    if (!failed(r1)) r1 else {
      if (inherits(r1,"tracker")) tracker_lines1 <- tracker_val(r1)
      else tracker_lines1 <- 1
      # cat("r1 lines:",tracker_lines1,"\n")
      set_LNR(init_lnr)
      r2 <- p2(x)
      if (!failed(r2)) r2 else {
        if (inherits(r2,"tracker")) tracker_lines2 <- tracker_val(r2)
        else tracker_lines2 <- 1
        # cat("r2 lines:",tracker_lines2,"\n")
        # cat("maximum lines parsed:", max(tracker_lines1, tracker_lines2),"\n")
        return(new_tracker(max(tracker_lines1, tracker_lines2)))
      }
    }
  }
}

# The then() function for error tracking
# need to keep track of how many lines are parsed. Every successful then we
# move one line further
`%thenE%` <- function(p1, p2) {
  function(x) {
    r1 <- p1(x)
    if (inherits(r1,"tracker")) {
      new_tracker(incby_LNR(r1))
    } else {
      if (failed(r1)) {
        new_tracker(LNR())
      } else {
        inc_LNR()
        r2 <- p2(r1$R)
        if (inherits(r2,"tracker")) {
          new_tracker(incby_LNR(r2))
        } else {
          if (failed(r2)) {
            new_tracker(LNR())
          } else {
            succeed(c(r1$L, r2$L))(r2$R)
          }
        }
      }
    }
  }
}

#Parser(literal("A") %thenE% literal("B"))(c("A","B"))

#Parser(literal("A") %thenE% (literal("B") %using% tolower))(c("A","B"))
#Parser(literal("A") %thenE% (literal("B") %using% tolower))(c("A","f"))
