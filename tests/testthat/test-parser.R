# Some predicate functions used in testing
starts_with_a <- function(x) grepl("^a",x[1])
starts_with_b <- function(x) grepl("^b",x[1])

test_that("'succeed' performs as it should", {
  expect_equal(succeed("abc")(c("xyz","def")), list(L="abc", R=c("xyz","def")))
})

test_that("'fail' performs as it should", {
  expect_equal(fail()(c("xyz","def")), list())
})

test_that("'satisfy' performs as it should", {
  expect_equal(satisfy(starts_with_a)(c("abc","def")), list(L="abc", R="def"))
  expect_equal(satisfy(starts_with_a)(c("bca","def")), list())
  expect_equal(satisfy(starts_with_a)(as.character(NULL)), list())
})

test_that("'literal' performs as it should", {
  expect_equal(literal("ab") (c("ab", "cdef")), list(L='ab', R='cdef'))
  expect_equal(literal("ab") (c("abc", "cdef")), list())
})

test_that("'%then%' performs as it should", {
  expect_equal((satisfy(starts_with_a) %then% satisfy(starts_with_b)) (c("ab", "bc", "de")), list(L=c("ab", "bc"), R="de"))
  expect_equal((satisfy(starts_with_a) %then% satisfy(starts_with_b)) (c("bb", "bc", "de")), list())
  expect_equal((satisfy(starts_with_a) %then% satisfy(starts_with_b)) (c("ab", "ac", "de")), list())
  expect_equal((literal('ab') %then% literal('ac')) (c("ab", "ac", "de")), list(L=c("ab", "ac"), R="de"))
  expect_equal((literal('ab') %then% literal('ac')) (as.character(0)), list())
})

test_that("'%or%' performs as it should", {
  expect_equal((literal("A") %or% literal("a"))(LETTERS[1:5]), list(L='A', R=LETTERS[2:5]))
  expect_equal((literal("A") %or% literal("a"))(letters[1:5]), list(L='a', R=letters[2:5]))
  expect_equal((literal("A") %or% literal("a"))(letters[2:5]), list())
  expect_equal((literal("a") %or% satisfy(starts_with_a)) (letters[1:5]), list(L='a', R=letters[2:5]))
  expect_equal((literal("a") %or% satisfy(starts_with_a)) (as.character(0)), list())
})

test_that("We can create a composite parser", {
  expect_equal((literal("a") %or% satisfy(starts_with_a) %or% literal("a")) (letters[1:5]), list(L='a', R=letters[2:5]))
  expect_equal(((literal("a") %or% satisfy(starts_with_a)) %then% (literal('b'))) (letters[1:5]), list(L=c('a','b'), R=letters[3:5]))
})

test_that("'zero.or.more' performs as it should", {
  expect_equal(zero.or.more(literal("A")) (LETTERS[1:5]), list(L="A", R=LETTERS[2:5]))
  expect_equal(zero.or.more(literal("A")) (LETTERS[2:5]), list(L=as.character(NULL), R=LETTERS[2:5]))
})

test_that("'one.or.more' performs as it should", {
  expect_equal(one.or.more(literal("A")) (LETTERS[1:5]), list(L="A", R=LETTERS[2:5]))
  expect_equal(one.or.more(literal("A")) (LETTERS[2:5]), list())
})

test_that("'exactly' performs as it should", {
  expect_equal(exactly(2,literal("A")) (c("A", LETTERS[1:5])), list(L=c("A","A"), R=LETTERS[2:5]))
  expect_equal(exactly(2,literal("A")) (c(rep("A",2), LETTERS[1:5])), list())
})

test_that("'match.n' performs as it should", {
  expect_equal(match.n(2,literal("A")) (c("A", LETTERS[1:5])), list(L=c("A","A"), R=LETTERS[2:5]))
  expect_equal(match.n(2,literal("A")) (c(rep("A",2), LETTERS[1:5])), list(L=c("A","A"), R=LETTERS[1:5]))
  expect_equal(match.n(2,literal("A")) (LETTERS[1:5]), list())
})

test_that("'Empty.line' performs as it should", {
  expect_equal(Empty.line (c("   \t  ", 'abc')), list(L="   \t  ", R='abc'))
})

test_that("'Spacer' performs as it should", {
  expect_equal(Spacer (c("   \t  ", "    ", "abc")), list(L=c("   \t  ", "    ") , R="abc"))
})

test_that("'Disposable' performs as it should", {
  expect_equal(Disposable() (c("   \t  ", "    ", "abc")), list(L=as.character(NULL) , R="abc"))
})
