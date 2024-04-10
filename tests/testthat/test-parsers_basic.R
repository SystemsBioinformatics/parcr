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
  expect_true(failed(fail()(c("xyz","def"))))
})

test_that("'satisfy' works in standard cases", {
  expect_equal(satisfy(starts_with_a)(c("abc","def")), list(L=list("abc"), R="def"))
  expect_true(failed(satisfy(starts_with_a)(c("bca","def"))))
  expect_true(failed(satisfy(starts_with_a)(character(0))))
})

test_that("'literal' works in standard cases", {
  expect_equal(literal("ab") (c("ab", "cdef")), list(L=list("ab"), R="cdef"))
  expect_true(failed(literal("ab") (c("abc", "cdef"))))
})

test_that("'eof' works in standard conditions", {
  expect_true(failed(eof()("a")))
  expect_equal(eof()(character(0)), list(L=list(), R=list()))
  expect_equal((literal("a") %then% eof())("a"), list(L=list("a"), R=list()))
})

test_that("`eof` works in repetition parsers", {
  expect_equal(zero_or_one(eof())(character(0)), list(L=list(), R=list()))
  expect_equal(zero_or_more(eof())(character(0)), list(L=list(), R=list()))
  expect_equal(one_or_more(eof())(character(0)), list(L=list(), R=list()))
  expect_equal(match_n(1,eof())(character(0)), list(L=list(), R=list()))
  expect_equal(exactly(1,eof())(character(0)), list(L=list(), R=list()))
})

test_that("'%then%' works in standard cases", {
  expect_equal((satisfy(starts_with_a) %then% satisfy(starts_with_b)) (c("ab", "bc", "de")), list(L=c(list("ab"), list("bc")), R="de"))
  expect_true(failed((satisfy(starts_with_a) %then% satisfy(starts_with_b))(c("bb", "bc", "de"))))
  expect_true(failed((satisfy(starts_with_a) %then% satisfy(starts_with_b))(c("ab", "ac", "de"))))
  expect_equal((literal('ab') %then% literal('ac')) (c("ab", "ac", "de")), list(L=c(list("ab"), list("ac")), R="de"))
  expect_true(failed((literal('ab') %then% literal('ac'))(as.character(0))))
})

test_that("'%xthen%' and '%thenx%' perform as they should", {
  expect_equal((satisfy(starts_with_a) %xthen% satisfy(starts_with_b)) (c("ab", "bc", "de")), list(L=list("ab"), R="de"))
  expect_equal((satisfy(starts_with_a) %thenx% satisfy(starts_with_b)) (c("ab", "bc", "de")), list(L=list("bc"), R="de"))
})

test_that("'%or%' works in standard cases", {
  expect_equal((literal("A") %or% literal("a"))(LETTERS[1:5]), list(L=list('A'), R=LETTERS[2:5]))
  expect_equal((literal("A") %or% literal("a"))(letters[1:5]), list(L=list('a'), R=letters[2:5]))
  expect_true(failed((literal("A") %or% literal("a"))(letters[2:5])))
  expect_equal((literal("a") %or% satisfy(starts_with_a)) (letters[1:5]), list(L=list('a'), R=letters[2:5]))
  expect_true(failed((literal("a") %or% satisfy(starts_with_a))(as.character(0))))
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
  expect_true(failed(one_or_more(literal("A"))(LETTERS[2:5])))
})

test_that("'exactly' works in standard cases", {
  expect_equal(exactly(2,literal("A")) (c("A", LETTERS[1:5])), list(L=c(list("A"),list("A")), R=LETTERS[2:5]))
  expect_true(failed(exactly(2,literal("A"))(c(rep("A",2), LETTERS[1:5]))))
  expect_equal(exactly(0,literal("A")) (LETTERS[2:5]), list(L=list(), R=LETTERS[2:5]))
  expect_equal(exactly(2,(literal("A") %ret% NULL))(rep("A",2)), list(L=list(), R=character(0)))
})

test_that("'zero_or_one' works in standard cases", {
  expect_equal(zero_or_one(literal("A")) (LETTERS[2:5]), list(L=list(), R=LETTERS[2:5]))
  expect_equal(zero_or_one(literal("A")) (LETTERS[1:5]), list(L=list("A"), R=LETTERS[2:5]))
  expect_true(failed(zero_or_one(literal("A"))(c("A",LETTERS[1:5]))))
})

test_that("'zero_or_one' is greedy", {
  ABblock <- function() {zero_or_one(literal("A")) %then% literal("B")}
  expect_equal(one_or_more(ABblock())(c("A","B","B")), list(L=list("A","B","B"), R=character(0)))
})

test_that("'match_n' works in standard cases", {
  expect_equal(match_n(2,literal("A")) (c("A", LETTERS[1:5])), list(L=c(list("A"),list("A")), R=LETTERS[2:5]))
  expect_equal(match_n(2,literal("A")) (c(rep("A",2), LETTERS[1:5])), list(L=c(list("A"),list("A")), R=LETTERS[1:5]))
  expect_true(failed(match_n(2,literal("A"))(LETTERS[1:5])))
})

test_that("'match_n' and 'exactly' wor when n=0", {
  expect_equal(match_n(0,literal("A"))("A"), list(L=list(), R="A"))
  expect_equal(exactly(0,literal("A"))("B"), list(L=list(), R="B"))
})

test_that("'match_s' works in standard cases", {
  expect_equal(match_s(get_numbers) ("12 13 14"), list(L=list(c(12,13,14)), R=character(0)))
  expect_true(failed(match_s(get_numbers) ("ab cd ef")))
})

test_that("All parsers accept character(0) input and return failure", {
  expect_true(failed(satisfy(is_empty_atom)(character(0))))
  expect_true(failed(literal(character(0))(character(0))))
  expect_true(failed((literal(character(0)) %or% literal(character(0)))(character(0))))
  expect_true(failed((literal("A") %then% literal(character(0)))("A")), list())
  expect_true(failed((literal(character(0)) %then% literal(character(0)))(character(0))))
  expect_true(failed((literal("A") %xthen% literal(character(0)))("A")))
  expect_true(failed((literal(character(0)) %xthen% literal(character(0)))(character(0))))
  expect_true(failed((literal("A") %thenx% literal(character(0)))("A")))
  expect_true((failed((literal(character(0)) %thenx% literal(character(0)))(character(0)))))
  expect_true(failed(match_s(get_numbers)(character(0))))
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

test_that("repeater parsers work when nested", {
  nested1 <- function() {exactly(2,literal("A")) %then% literal("B")}
  nested2 <- function() {one_or_more(nested1())}
  expect_equal(nested1()(c("A","A","B","A","A","B")), list(L=c(list("A"),list("A"),list("B")), R=c("A","A","B")))
  expect_equal(one_or_more(nested1())(c("A","A","B","A","A","B")), list(L=c(list("A"),list("A"),list("B"),list("A"),list("A"),list("B")), R=character(0)))
  expect_equal(zero_or_one(nested1())(c("A","A","B")), list(L=c(list("A"),list("A"),list("B")), R=character(0)))
  expect_equal(zero_or_one(nested1())(c("B","A","B")), list(L=list(), R=c("B","A","B")))
  expect_equal(exactly(2,nested1())(c("A","A","B","A","A","B")), list(L=c(list("A"),list("A"),list("B"),list("A"),list("A"),list("B")), R=character(0)))
  nested3 <- function() {literal("A") %then% literal("A") %then% literal("B")}
  nested4 <- function() {one_or_more(nested3())}
  expect_equal(nested3()(c("A","A","B","A","A","B")), list(L=c(list("A"),list("A"),list("B")), R=c("A","A","B")))
  expect_equal(one_or_more(nested4())(c("A","A","B","A","A","B")), list(L=c(list("A"),list("A"),list("B"),list("A"),list("A"),list("B")), R=character(0)))
})

p1 <- function(x) {(literal("A") %then% literal("B"))(x)}
p2 <- function(x) {(literal("A") %then% literal("C") %then% literal("D"))(x)}
p7 <- function(x) {(literal("A") %then% literal("C") %then% literal("E") %then% literal("D"))(x)}
p3 <- function(x) {(EmptyLine() %then% EmptyLine() %then% literal("A"))(x)}
p4 <- function(x) {(exactly(2,EmptyLine()) %then% literal("A"))(x)}
p5 <- function(x) {(one_or_more(EmptyLine()) %then% literal("A"))(x)}
p6 <- function(x) {(MaybeEmpty() %then% literal("A"))(x)}

test_that("'reporter' works in successful parses", {
  expect_equal(reporter((p1 %or% p2) %then% eof())(c("A","C","D")), c(list("A"), list("C"), list("D")))
})

test_that("'reporter()' yields error messages", {
  expect_error(reporter(literal("A"))("B"), regexp="line 1")
  expect_error(reporter(p1)(c("A","C")), regexp="line 2")
  expect_error(reporter(p2)(c("A","C","E")), regexp="line 3")
  expect_error(reporter(literal("A") %or% literal("B"))("C"), regexp = "line 1")
  expect_error(reporter(p2 %or% p1)(c("A","C","E")), regexp = "line 3")
  expect_error(reporter(p1 %or% p2)(c("A","C","E")), regexp = "line 3")
  expect_error(reporter(p3)(c("","","C")), regexp = "line 3")
  expect_error(reporter(p4)(c("","","C")), regexp = "line 3")
  expect_error(reporter(p5)(c("","","C")), regexp = "line 3")
  expect_error(reporter(p6)(c("","","C")), regexp = "line 3")
})

test_that("'reporter()' yields warning when not completely consuming input", {
  expect_warning(reporter(p2)(c("A","C","D")), "did not completely consume")
  expect_warning(reporter((literal("B") %or% literal("A")))(c("A","B")), "did not completely consume")
  expect_warning(reporter(p1)(c("A","B")), "did not completely consume")
  expect_warning(reporter(match_n(1,literal("A")))(c("A","B","A")), "did not completely consume")
})

test_that("All combinations of 3 alternative, exclusive parsers yield same error",{
  expect_error(reporter((p1 %or% p2 %or% p7))(c("A","C","E","F")), regexp = "line 4")
  expect_error(reporter((p1 %or% p7 %or% p2))(c("A","C","E","F")), regexp = "line 4")
  expect_error(reporter((p7 %or% p1 %or% p2))(c("A","C","E","F")), regexp = "line 4")
  expect_error(reporter((p7 %or% p2 %or% p1))(c("A","C","E","F")), regexp = "line 4")
  expect_error(reporter((p2 %or% p1 %or% p7))(c("A","C","E","F")), regexp = "line 4")
  expect_error(reporter((p2 %or% p7 %or% p1))(c("A","C","E","F")), regexp = "line 4")
})

test_that("'match_n' yields correct error", {
  expect_error(reporter(match_n(2,literal("A")))(c("A","B","A")), regexp = "line 2")
  expect_error(reporter(match_n(5,literal("A")))(c("A","A","A","A","B")), regexp = "line 5")
})



