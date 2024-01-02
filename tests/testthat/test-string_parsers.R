test_that("'split.by' works in standard cases", {
  expect_equal(split_by((literal("a") %then% literal("b")),"\\t") ("a\tb"), list(L=c(list("a"),list("b")), R=character(0)))
  expect_equal(split_by((literal("a") %then% literal("b")),"\\t") ("a\tb\tc"), list())
  expect_equal(split_by((literal("a") %then% literal("b")), "\\t", finish=FALSE) ("a\tb\tc"), list(L=c(list("a"),list("b")), R=character(0)))
})
