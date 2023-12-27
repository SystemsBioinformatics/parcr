test_that("'is.empty' works", {
  expect_true(is.empty(list()))
  expect_false(is.empty(list(x='ab')))
})
