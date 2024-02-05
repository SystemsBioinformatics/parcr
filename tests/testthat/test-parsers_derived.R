test_that("'EmptyLine' works in standard cases", {
  expect_equal(EmptyLine() (c("   \t  ", 'abc')), list(L=list("   \t  "), R='abc'))
  expect_equal(EmptyLine() (" "), list(L=list(" "), R=character(0)))
  expect_equal(EmptyLine() (""), list(L=list(""), R=character(0)))
  expect_true(failed(EmptyLine() ("   .")))
})

test_that("'Spacer' works in standard cases", {
  expect_equal(Spacer() (c("   \t  ", "    ", "abc")), list(L=list() , R="abc"))
})

test_that("'MaybeEmpty' works in standard cases", {
  expect_equal(MaybeEmpty() (c("   \t  ", "    ", "abc")), list(L=list() , R="abc"))
  expect_equal(MaybeEmpty() ("abc"), list(L=list() , R="abc"))
})

test_that("Parsers can consume the input up to the end", {
  expect_equal(EmptyLine() (c(" ")), list(L=list(" "), R=character(0)))
  expect_equal(Spacer() (c(" "," ")), list(L=list(), R=character(0)))
})

test_that("'Ignore' works", {
  expect_equal(Ignore()(LETTERS), list(L=list(), R=character(0)))
})
