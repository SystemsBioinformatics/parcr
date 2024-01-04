test_that("'is_empty_atom' and 'failed' work", {
  expect_true(is_empty_atom(character(0)))
  expect_false(is_empty_atom('ab'))
  expect_false(is_empty_atom(list()))
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
