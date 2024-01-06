starts_with_a <- function(x) grepl("^a",x[1])

test_that("'by_split' works in standard cases", {
  expect_equal(by_split((literal("a") %then% literal("b")),"\\t") ("a\tb"), list(L=c(list("a"),list("b")), R=character(0)))
  expect_true(failed(by_split((literal("a") %then% literal("b")),"\\t")("a\tb\tc")))
  expect_equal(by_split((literal("a") %then% literal("b")), "\\t", finish=FALSE) ("a\tb\tc"), list(L=c(list("a"),list("b")), R=character(0)))
  expect_equal(by_split((literal("a") %then% literal("b")), "") ("ab"), list(L=c(list("a"),list("b")), R=character(0)))
  expect_equal(by_split(exactly(3,literal("a")), "\\t") ("a\ta\ta"), list(L=c(list("a"),list("a"), list("a")), R=character(0)))
  expect_equal(by_split(exactly(3,satisfy(starts_with_a)), ",", fixed=TRUE)('atggc,acggg,acttg'), list(L=c(list("atggc"), list("acggg"), list("acttg")), R=character(0)))
})

test_that("'by_symbol' works in standard cases", {
  expect_equal(by_symbol((literal("a") %then% literal("b"))) ("ab"), list(L=c(list("a"),list("b")), R=character(0)))
  expect_equal(by_symbol((literal("a") %then% literal("b")), finish=FALSE) ("abc"), list(L=c(list("a"),list("b")), R=character(0)))
  expect_true(failed(by_symbol((literal("a") %then% literal("b")))("abc")))
  expect_equal(by_symbol(exactly(3,literal("a"))) ("aaa"), list(L=c(list("a"),list("a"), list("a")), R=character(0)))
})

test_that("'by_split' and 'by_symbol' trackers stay on current line", {
  reset_LNR()
  d <- by_split(exactly(3,literal("a")), "\\t")("a\ta\ta")
  expect_identical(LNR(),1L)
  set_LNR(5)
  expect_identical(marker_val(by_split(exactly(3,literal("a")), "\\t")("a\ta\ta\ta")),5L)
  expect_identical(marker_val(by_symbol(exactly(3,literal("a"))) ("aa")),5L)
  set_LNR(2)
  expect_identical(marker_val((literal("A") %then% by_symbol(exactly(3,literal("a"))))(c("A","aa"))), 3L)
})
