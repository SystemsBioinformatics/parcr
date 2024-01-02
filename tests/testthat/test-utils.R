test_that("'is.empty' and 'failed' work", {
  expect_true(is.empty(character(0)))
  expect_false(is.empty('ab'))
  expect_true(failed(list()))
  expect_false(failed(list('ab')))
  expect_false(failed(character(0)))
  expect_false(failed('ab'))
  expect_false(failed(character(0)))
  expect_false(failed('ab'))
})

test_that("'ensure.list' works", {
  expect_equal(ensure.list(data.frame(a='a')), list(data.frame(a='a')))
})
