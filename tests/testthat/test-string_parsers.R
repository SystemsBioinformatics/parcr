test_that("'by_split' works in standard cases", {
  expect_equal(by_split((literal("a") %then% literal("b")),"\\t") ("a\tb"), list(L=c(list("a"),list("b")), R=character(0)))
  expect_equal(by_split((literal("a") %then% literal("b")),"\\t") ("a\tb\tc"), list())
  expect_equal(by_split((literal("a") %then% literal("b")), "\\t", finish=FALSE) ("a\tb\tc"), list(L=c(list("a"),list("b")), R=character(0)))
  expect_equal(by_split((literal("a") %then% literal("b")), "") ("ab"), list(L=c(list("a"),list("b")), R=character(0)))
  expect_equal(by_split(exactly(3,literal("a")), "\\t") ("a\ta\ta"), list(L=c(list("a"),list("a"), list("a")), R=character(0)))
})

test_that("'by_symbol' works in standard cases", {
  expect_equal(by_symbol((literal("a") %then% literal("b"))) ("ab"), list(L=c(list("a"),list("b")), R=character(0)))
  expect_equal(by_symbol((literal("a") %then% literal("b")), finish=FALSE) ("abc"), list(L=c(list("a"),list("b")), R=character(0)))
  expect_equal(by_symbol((literal("a") %then% literal("b"))) ("abc"), list())
  expect_equal(by_symbol(exactly(3,literal("a"))) ("aaa"), list(L=c(list("a"),list("a"), list("a")), R=character(0)))
})
