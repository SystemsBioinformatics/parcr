test_that("'EmptyLine' works in standard cases", {
  expect_equal(EmptyLine() (c("   \t  ", 'abc')), list(L=list("   \t  "), R='abc'))
  expect_equal(EmptyLine() (" "), list(L=list(" "), R=character(0)))
  expect_equal(EmptyLine() (""), list(L=list(""), R=character(0)))
  expect_equal(EmptyLine() ("   ."), list())
})

test_that("'Spacer' works in standard cases", {
  expect_equal(Spacer() (c("   \t  ", "    ", "abc")), list(L=list() , R="abc"))
})

test_that("'MaybeEmpty' works in standard cases", {
  expect_equal(MaybeEmpty() (c("   \t  ", "    ", "abc")), list(L=list() , R="abc"))
  expect_equal(MaybeEmpty() ("abc"), list(L=list() , R="abc"))
})

# test_that("'Numbers' works in standard cases", {
#   expect_equal(Numbers(3) ('1  2  3'), list(L=list(1:3), R=character(0)))
#   expect_equal(Numbers(3) ('10\t20\t30'), list(L=list(c(10,20,30)), R=character(0)))
#   expect_equal(Numbers(4) ('10\t20\t30'), list())
#   expect_equal(Numbers(0) (" "), list(L=list(), R=character(0)))
#   expect_equal(Numbers(6) (paste(as.character(1:6),collapse="\t")), list(L=list(1:6), R=character(0)))
#   expect_equal(Numbers(6) (rep(paste(as.character(1:6),collapse="\t"), 2)), list(L=list(1:6), R=paste(as.character(1:6),collapse="\t")))
#   expect_equal(exactly(2, Numbers(6)) (rep(paste(as.character(1:6),collapse="\t"), 2)), list(L=c(list(1:6), list(1:6)), R=character(0)))
# })
#
# test_that("All parsers accept character(0) input", {
#   expect_equal(Numbers(1) (character(0)), list())
# })

test_that("Parsers can consume the input up to the end", {
  expect_equal(EmptyLine() (c(" ")), list(L=list(" "), R=character(0)))
  expect_equal(Spacer() (c(" "," ")), list(L=list(), R=character(0)))
})
