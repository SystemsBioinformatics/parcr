# Some predicate functions used in testing
starts_with_a <- function(x) grepl("^a",x[1])
starts_with_b <- function(x) grepl("^b",x[1])
starts_with_hash <- function(x) grepl("^#",x[1])

get_numbers <- function(x) {
  if (length(x)==0) list()
  else {
    m <- gregexpr("[[:digit:]]+", x)
    matches <- as.numeric(regmatches(x,m)[[1]])
    if (length(matches)==0) list() else matches
  }
}

test_that("'succeed' works in standard cases", {
  expect_equal(succeed("abc")(c("xyz","def")), list(L=list("abc"), R=c("xyz","def")))
  expect_equal(succeed(data.frame(title="The beginning", author="J. Doe"))(c("Unconsumed","text")),
               list(L=list(data.frame(title="The beginning", author="J. Doe")), R=c("Unconsumed","text")))
})

test_that("'fail' works in standard cases", {
  expect_equal(fail()(c("xyz","def")), list())
})

test_that("'satisfy' works in standard cases", {
  expect_equal(satisfy(starts_with_a)(c("abc","def")), list(L=list("abc"), R="def"))
  expect_equal(satisfy(starts_with_a)(c("bca","def")), list())
  expect_equal(satisfy(starts_with_a)(character(0)), list())
})

test_that("'literal' works in standard cases", {
  expect_equal(literal("ab") (c("ab", "cdef")), list(L=list("ab"), R="cdef"))
  expect_equal(literal("ab") (c("abc", "cdef")), list())
})

test_that("'eof' works in standard conditions", {
  expect_equal(eof()("a"), list())
  expect_equal(eof()(character(0)), list(L=list(), R=list()))
  expect_equal((literal("a") %then% eof())("a"), list(L=list("a"), R=list()))
})

test_that("No racing condition can be induced by `eof`", {
  expect_equal(one_or_more(eof())(character(0)), list())
  expect_equal(exactly(1,eof())(character(0)), list())
})

test_that("'%then%' works in standard cases", {
  expect_equal((satisfy(starts_with_a) %then% satisfy(starts_with_b)) (c("ab", "bc", "de")), list(L=c(list("ab"), list("bc")), R="de"))
  expect_equal((satisfy(starts_with_a) %then% satisfy(starts_with_b)) (c("bb", "bc", "de")), list())
  expect_equal((satisfy(starts_with_a) %then% satisfy(starts_with_b)) (c("ab", "ac", "de")), list())
  expect_equal((literal('ab') %then% literal('ac')) (c("ab", "ac", "de")), list(L=c(list("ab"), list("ac")), R="de"))
  expect_equal((literal('ab') %then% literal('ac')) (as.character(0)), list())
})

test_that("'%xthen%' and '%thenx%' perform as they should", {
  expect_equal((satisfy(starts_with_a) %xthen% satisfy(starts_with_b)) (c("ab", "bc", "de")), list(L=list("ab"), R="de"))
  expect_equal((satisfy(starts_with_a) %thenx% satisfy(starts_with_b)) (c("ab", "bc", "de")), list(L=list("bc"), R="de"))
})

test_that("'%or%' works in standard cases", {
  expect_equal((literal("A") %or% literal("a"))(LETTERS[1:5]), list(L=list('A'), R=LETTERS[2:5]))
  expect_equal((literal("A") %or% literal("a"))(letters[1:5]), list(L=list('a'), R=letters[2:5]))
  expect_equal((literal("A") %or% literal("a"))(letters[2:5]), list())
  expect_equal((literal("a") %or% satisfy(starts_with_a)) (letters[1:5]), list(L=list('a'), R=letters[2:5]))
  expect_equal((literal("a") %or% satisfy(starts_with_a)) (as.character(0)), list())
})

test_that("We can create a composite parser", {
  expect_equal((literal("a") %or% satisfy(starts_with_a) %or% literal("a")) (letters[1:5]), list(L=list('a'), R=letters[2:5]))
  expect_equal(((literal("a") %or% satisfy(starts_with_a)) %then% (literal('b'))) (letters[1:5]), list(L=c(list('a'),list('b')), R=letters[3:5]))
})

test_that("'zero_or_more' works in standard cases", {
  expect_equal(zero_or_more(literal("A")) (LETTERS[1:5]), list(L=list("A"), R=LETTERS[2:5]))
  expect_equal(zero_or_more(literal("A")) (LETTERS[2:5]), list(L=list(), R=LETTERS[2:5]))
})

test_that("'one_or_more' works in standard cases", {
  expect_equal(one_or_more(literal("A")) (LETTERS[1:5]), list(L=list("A"), R=LETTERS[2:5]))
  expect_equal(one_or_more(literal("A")) (LETTERS[2:5]), list())
})

test_that("'exactly' works in standard cases", {
  expect_equal(exactly(2,literal("A")) (c("A", LETTERS[1:5])), list(L=c(list("A"),list("A")), R=LETTERS[2:5]))
  expect_equal(exactly(2,literal("A")) (c(rep("A",2), LETTERS[1:5])), list())
  expect_equal(exactly(0,literal("A")) (LETTERS[2:5]), list(L=list(), R=LETTERS[2:5]))
})

test_that("'zero_or_one' works in standard cases", {
  expect_equal(zero_or_one(literal("A")) (LETTERS[2:5]), list(L=list(), R=LETTERS[2:5]))
  expect_equal(zero_or_one(literal("A")) (LETTERS[1:5]), list(L=list("A"), R=LETTERS[2:5]))
  expect_equal(zero_or_one(literal("A")) (c("A",LETTERS[1:5])), list())
})

test_that("'match_n' works in standard cases", {
  expect_equal(match_n(2,literal("A")) (c("A", LETTERS[1:5])), list(L=c(list("A"),list("A")), R=LETTERS[2:5]))
  expect_equal(match_n(2,literal("A")) (c(rep("A",2), LETTERS[1:5])), list(L=c(list("A"),list("A")), R=LETTERS[1:5]))
  expect_equal(match_n(2,literal("A")) (LETTERS[1:5]), list())
})

test_that("'match_n' and 'exactly' wor when n=0", {
  expect_equal(match_n(0,literal("A"))("A"), list(L=list(), R="A"))
  expect_equal(exactly(0,literal("A"))("B"), list(L=list(), R="B"))
})

test_that("'match_s' works in standard cases", {
  expect_equal(match_s(get_numbers) ("12 13 14"), list(L=list(c(12,13,14)), R=character(0)))
  expect_equal(match_s(get_numbers) ("ab cd ef"), list())
})

test_that("All parsers accept character(0) input and return failure", {
  expect_equal(satisfy(is_empty_atom) (character(0)), list())
  expect_equal(literal(character(0)) (character(0)), list())
  expect_equal((literal(character(0)) %or% literal(character(0))) (character(0)), list())
  expect_equal((literal("A") %then% literal(character(0))) ("A"), list())
  expect_equal((literal(character(0)) %then% literal(character(0))) (character(0)), list())
  expect_equal((literal("A") %xthen% literal(character(0))) ("A"), list())
  expect_equal((literal(character(0)) %xthen% literal(character(0))) (character(0)), list())
  expect_equal((literal("A") %thenx% literal(character(0))) ("A"), list())
  expect_equal((literal(character(0)) %thenx% literal(character(0))) (character(0)), list())
  expect_equal(match_s(get_numbers) (character(0)), list())
})

test_that("Parsers can consume the input up to the end", {
  expect_equal((literal("A") %then% literal("B")) (c("A","B")), list(L=c(list("A"), list("B")), R=character(0)))
  expect_equal((literal("A") %then% (literal("b") %or% literal("B"))) (c("A","b")), list(L=c(list("A"),list("b")), R=character(0)))
  expect_equal((literal("A") %xthen% literal("B")) (c("A","B")), list(L=list("A"), R=character(0)))
  expect_equal((literal("A") %thenx% literal("B")) (c("A","B")), list(L=list("B"), R=character(0)))
  expect_equal((literal("A") %xthen% (literal("b") %or% literal("B"))) (c("A","b")), list(L=list("A"), R=character(0)))
  expect_equal((zero_or_more(literal("A"))) (rep("A",5)), list(L=c(list("A"),list("A"),list("A"),list("A"),list("A")), R=character(0)))
  expect_equal((zero_or_more(literal("A"))) (c("")), list(L=list(), R=""))
  expect_equal((zero_or_more(literal("A"))) (character(0)), list(L=list(), R=character(0)))
  expect_equal((one_or_more(literal("A"))) (rep("A",5)), list(L=c(list("A"),list("A"),list("A"),list("A"),list("A")), R=character(0)))
  expect_equal((one_or_more(literal("A"))) (c("A")), list(L=list("A"), R=character(0)))
  expect_equal((exactly(2,literal("A"))) (c("A","A")), list(L=c(list("A"),list("A")), R=character(0)))
  expect_equal((match_n(2,literal("A"))) (c("A","A")), list(L=c(list("A"),list("A")), R=character(0)))
  expect_equal(Spacer() (c(" "," ")), list(L=list(), R=character(0)))
})
