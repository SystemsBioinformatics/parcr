test_that("'is.empty' works", {
  expect_true(is.empty(list()))
  expect_false(is.empty(list(x='ab')))
})

test_that("List aggregation works with non-empty lists", {
  expect_equal(agg(list('abc','def'), list('ghi','jkl')), list(list('abc','def'), list('ghi','jkl')))
})

test_that("'list()' is the unit element of list aggregation", {
  expect_equal(agg(list('abc','def'), list()), list(list('abc','def')))
  expect_equal(agg(list(), list('abc','def')), list(list('abc','def')))
  expect_equal(agg(list(), list()), list())
})
