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

test_that("'fst' and 'snd' work", {
  expect_equal(fst(c('a','b')), 'a')
  expect_equal(snd(c('a','b')), 'b')
  expect_equal(fst(c('a')), 'a')
  expect_equal(snd(c('a')), NULL)
})

test_that("'ensure.list' works", {
  expect_equal(ensure.list(data.frame(a='a')), list(data.frame(a='a')))
})
