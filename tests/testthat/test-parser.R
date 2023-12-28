# Some predicate functions used in testing
starts_with_a <- function(x) grepl("^a",x[1])
starts_with_b <- function(x) grepl("^b",x[1])
length_zero <- function(x) length(x)==0

test_that("'succeed' works in standard cases", {
  expect_equal(succeed("abc")(c("xyz","def")), list(L="abc", R=c("xyz","def")))
})

test_that("'fail' works in standard cases", {
  expect_equal(fail()(c("xyz","def")), list())
})

test_that("'satisfy' works in standard cases", {
  expect_equal(satisfy(starts_with_a)(c("abc","def")), list(L="abc", R="def"))
  expect_equal(satisfy(starts_with_a)(c("bca","def")), list())
  expect_equal(satisfy(starts_with_a)(as.character(NULL)), list())
})

test_that("'literal' works in standard cases", {
  expect_equal(literal("ab") (c("ab", "cdef")), list(L='ab', R='cdef'))
  expect_equal(literal("ab") (c("abc", "cdef")), list())
})

test_that("'%then%' works in standard cases", {
  expect_equal((satisfy(starts_with_a) %then% satisfy(starts_with_b)) (c("ab", "bc", "de")), list(L=c("ab", "bc"), R="de"))
  expect_equal((satisfy(starts_with_a) %then% satisfy(starts_with_b)) (c("bb", "bc", "de")), list())
  expect_equal((satisfy(starts_with_a) %then% satisfy(starts_with_b)) (c("ab", "ac", "de")), list())
  expect_equal((literal('ab') %then% literal('ac')) (c("ab", "ac", "de")), list(L=c("ab", "ac"), R="de"))
  expect_equal((literal('ab') %then% literal('ac')) (as.character(0)), list())
})

test_that("'%xthen%' and '%thenx%' perform as they should", {
  expect_equal((satisfy(starts_with_a) %xthen% satisfy(starts_with_b)) (c("ab", "bc", "de")), list(L=c("ab"), R="de"))
  expect_equal((satisfy(starts_with_a) %thenx% satisfy(starts_with_b)) (c("ab", "bc", "de")), list(L=c("bc"), R="de"))
})

test_that("'%or%' works in standard cases", {
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

test_that("'zero.or.more' works in standard cases", {
  expect_equal(zero.or.more(literal("A")) (LETTERS[1:5]), list(L="A", R=LETTERS[2:5]))
  expect_equal(zero.or.more(literal("A")) (LETTERS[2:5]), list(L=as.character(NULL), R=LETTERS[2:5]))
})

test_that("'one.or.more' works in standard cases", {
  expect_equal(one.or.more(literal("A")) (LETTERS[1:5]), list(L="A", R=LETTERS[2:5]))
  expect_equal(one.or.more(literal("A")) (LETTERS[2:5]), list())
})

test_that("'exactly' works in standard cases", {
  expect_equal(exactly(2,literal("A")) (c("A", LETTERS[1:5])), list(L=c("A","A"), R=LETTERS[2:5]))
  expect_equal(exactly(2,literal("A")) (c(rep("A",2), LETTERS[1:5])), list())
})

test_that("'match.n' works in standard cases", {
  expect_equal(match.n(2,literal("A")) (c("A", LETTERS[1:5])), list(L=c("A","A"), R=LETTERS[2:5]))
  expect_equal(match.n(2,literal("A")) (c(rep("A",2), LETTERS[1:5])), list(L=c("A","A"), R=LETTERS[1:5]))
  expect_equal(match.n(2,literal("A")) (LETTERS[1:5]), list())
})

test_that("'Empty.line' works in standard cases", {
  expect_equal(Empty.line (c("   \t  ", 'abc')), list(L="   \t  ", R='abc'))
})

test_that("'Spacer' works in standard cases", {
  expect_equal(Spacer (c("   \t  ", "    ", "abc")), list(L=c("   \t  ", "    ") , R="abc"))
})

test_that("'Disposable' works in standard cases", {
  expect_equal(Disposable() (c("   \t  ", "    ", "abc")), list(L=as.character(NULL) , R="abc"))
})

test_that("All parsers accept character(0) input", {
  expect_equal(satisfy(length_zero) (character(0)), list(L=character(0), R=character(0)))
  expect_equal(literal(character(0)) (character(0)), list(L=character(0), R=character(0)))
  expect_equal((literal(character(0)) %or% literal(character(0))) (character(0)), list(L=character(0), R=character(0)))
  expect_equal((literal("A") %then% literal(character(0))) ("A"), list(L="A", R=character(0)))
  expect_equal((literal(character(0)) %then% literal(character(0))) (character(0)), list())
  expect_equal((literal("A") %xthen% literal(character(0))) ("A"), list(L="A", R=character(0)))
  expect_equal((literal(character(0)) %xthen% literal(character(0))) (character(0)), list())
  expect_equal((literal("A") %thenx% literal(character(0))) ("A"), list(L=character(0), R=character(0)))
  expect_equal((literal(character(0)) %thenx% literal(character(0))) (character(0)), list())
})

test_that("Parsers can consume the input up to the end", {
  expect_equal((literal("A") %then% literal("B")) (c("A","B")), list(L=c("A","B"), R=character(0)))
  expect_equal((literal("A") %then% (literal("b") %or% literal("B"))) (c("A","b")), list(L=c("A","b"), R=character(0)))
  expect_equal((literal("A") %xthen% literal("B")) (c("A","B")), list(L=c("A"), R=character(0)))
  expect_equal((literal("A") %thenx% literal("B")) (c("A","B")), list(L=c("B"), R=character(0)))
  expect_equal((literal("A") %xthen% (literal("b") %or% literal("B"))) (c("A","b")), list(L=c("A"), R=character(0)))
  expect_equal((zero.or.more(literal("A"))) (rep("A",5)), list(L=rep("A",5), R=character(0)))
  expect_equal((zero.or.more(literal("A"))) (c("")), list(L=character(0), R=""))
  expect_equal((zero.or.more(literal("A"))) (character(0)), list(L=character(0), R=character(0)))
  expect_equal((one.or.more(literal("A"))) (rep("A",5)), list(L=rep("A",5), R=character(0)))
  expect_equal((one.or.more(literal("A"))) (c("A")), list(L="A", R=character(0)))
  expect_equal((exactly(2,literal("A"))) (c("A","A")), list(L=c("A","A"), R=character(0)))
  expect_equal((match.n(2,literal("A"))) (c("A","A")), list(L=c("A","A"), R=character(0)))
  expect_equal(Empty.line (c(" ")), list(L=" ", R=character(0)))
  expect_equal(Spacer (c(" "," ")), list(L=c(" "," "), R=character(0)))
  expect_equal(Disposable() (c(" "," ")), list(L=character(0), R=character(0)))
})
