test_that("storage and retrieval basics work", {
  expect_no_error(store("a", 1))
  expect_equal(retrieve("a"), 1)
})

test_that("storage and retrieval in a parser works", {
  parse_nr <- function(line) {
    m <- stringr::str_extract(line, "NUMBER=(\\d+)", group=1)
    if (is.na(m)) list()
    else {
      N <- as.numeric(m)
      store("N",N)
    }
  }
  p <- function() {match_s(parse_nr) %then% exactly(retrieve("N"), literal("A"))}
  expect_equal((p()(c("NUMBER=3", "A", "A", "A")))[["R"]], character(0))
})
