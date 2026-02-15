# Tests for expected element functionality in error messages

test_that("'literal' parser includes expected in error marker", {
  reset_LNR()
  r <- literal("A")("B")
  expect_true(failed(r))
  expect_identical(marker_expected(r), "literal 'A'")
})

test_that("'eof' parser includes expected in error marker", {
  reset_LNR()
  r <- eof()("A")
  expect_true(failed(r))
  expect_identical(marker_expected(r), "end of file")
})

test_that("'satisfy' parser includes expected in error marker", {
  reset_LNR()
  starts_with_a <- function(x) grepl("^a", x)
  r <- satisfy(starts_with_a)("B")
  expect_true(failed(r))
  expect_identical(marker_expected(r), "matching input")
})

test_that("'satisfy' parser can have custom expected message", {
  reset_LNR()
  is_number <- function(x) grepl("^\\d+$", x)
  r <- satisfy(is_number, expected = "numeric value")("abc")
  expect_true(failed(r))
  expect_identical(marker_expected(r), "numeric value")
})

test_that("'%or%' combinator merges expected values when both fail at same position", {
  reset_LNR()
  r <- (literal("A") %or% literal("B"))("C")
  expect_true(failed(r))
  expected <- marker_expected(r)
  expect_true("literal 'A'" %in% expected)
  expect_true("literal 'B'" %in% expected)
  expect_equal(length(expected), 2)
})

test_that("'%or%' combinator returns expected from furthest failure", {
  reset_LNR()
  p1 <- literal("A") %then% literal("B")
  p2 <- literal("A") %then% literal("C")
  r <- (p1 %or% p2)(c("A", "D"))
  expect_true(failed(r))
  expected <- marker_expected(r)
  # Both parsers fail on line 2, so both expectations should be included
  expect_true("literal 'B'" %in% expected)
  expect_true("literal 'C'" %in% expected)
})

test_that("'%or%' combinator with three alternatives merges all expected at same position", {
  reset_LNR()
  r <- (literal("A") %or% literal("B") %or% literal("C"))("D")
  expect_true(failed(r))
  expected <- marker_expected(r)
  expect_true("literal 'A'" %in% expected)
  expect_true("literal 'B'" %in% expected)
  expect_true("literal 'C'" %in% expected)
})

test_that("'%then%' combinator propagates expected from first parser", {
  reset_LNR()
  r <- (literal("A") %then% literal("B"))("C")
  expect_true(failed(r))
  expect_identical(marker_expected(r), "literal 'A'")
})

test_that("'%then%' combinator propagates expected from second parser", {
  reset_LNR()
  r <- (literal("A") %then% literal("B"))(c("A", "C"))
  expect_true(failed(r))
  expect_identical(marker_expected(r), "literal 'B'")
})

test_that("'%xthen%' combinator propagates expected information", {
  reset_LNR()
  r <- (literal("A") %xthen% literal("B"))(c("A", "C"))
  expect_true(failed(r))
  expect_identical(marker_expected(r), "literal 'B'")
})

test_that("'%thenx%' combinator propagates expected information", {
  reset_LNR()
  r <- (literal("A") %thenx% literal("B"))(c("A", "C"))
  expect_true(failed(r))
  expect_identical(marker_expected(r), "literal 'B'")
})

test_that("'named' parser adds semantic name to error marker", {
  reset_LNR()
  nucleotide <- named(
    satisfy(function(x) grepl("^[GATC]+$", x)),
    "nucleotide sequence"
  )
  r <- nucleotide("XYZ")
  expect_true(failed(r))
  expect_identical(marker_expected(r), "nucleotide sequence")
})

test_that("'named' parser replaces underlying expected value", {
  reset_LNR()
  # literal would normally say "literal 'A'", but named overrides it
  named_parser <- named(literal("A"), "start token")
  r <- named_parser("B")
  expect_true(failed(r))
  expect_identical(marker_expected(r), "start token")
})

test_that("'named' parsers combine nicely with %or%", {
  reset_LNR()
  p1 <- named(literal("A"), "token A")
  p2 <- named(literal("B"), "token B")
  r <- (p1 %or% p2)("C")
  expect_true(failed(r))
  expected <- marker_expected(r)
  expect_true("token A" %in% expected)
  expect_true("token B" %in% expected)
})

test_that("'reporter' includes expected in error message for single expectation", {
  reset_LNR()
  expect_error(
    reporter(literal("A"))("B"),
    regexp = "Expected: literal 'A'"
  )
})

test_that("'reporter' includes expected in error message for multiple expectations", {
  reset_LNR()
  expect_error(
    reporter(literal("A") %or% literal("B"))("C"),
    regexp = "Expected one of:.*literal 'A'.*literal 'B'"
  )
})

test_that("'reporter' error message shows correct line number and expected", {
  reset_LNR()
  p <- literal("A") %then% literal("B") %then% literal("C")
  err <- tryCatch(
    reporter(p)(c("A", "B", "D")),
    error = function(e) e
  )
  expect_match(err$message, "line 3")
  expect_match(err$message, "Expected: literal 'C'")
})

test_that("'reporter' with %or% shows combined expectations at failure point", {
  reset_LNR()
  nucleotide <- named(
    satisfy(function(x) grepl("^[GATC]+$", x)),
    "nucleotide sequence"
  )
  protein <- named(
    satisfy(function(x) grepl("^[ARNDCQEGHILKMFPSTWYV]+$", x)),
    "protein sequence"
  )
  p <- nucleotide %or% protein
  err <- tryCatch(
    reporter(p %then% eof())(c("12345")),
    error = function(e) e
  )
  expect_match(err$message, "Expected one of:")
  expect_match(err$message, "nucleotide sequence")
  expect_match(err$message, "protein sequence")
})

test_that("'reporter' error object contains expected attribute", {
  reset_LNR()
  err <- tryCatch(
    reporter(literal("A") %or% literal("B"))("C"),
    error = function(e) e
  )
  expect_true(!is.null(err$expected))
  expect_true("literal 'A'" %in% err$expected)
  expect_true("literal 'B'" %in% err$expected)
})

test_that("complex parser with nested %or% produces meaningful expectations", {
  reset_LNR()
  # Simulating a fasta header or sequence line
  header <- named(
    satisfy(function(x) grepl("^>", x)),
    "header line (starting with '>')"
  )
  nucl_seq <- named(
    satisfy(function(x) grepl("^[GATC]+$", x)),
    "nucleotide sequence"
  )
  prot_seq <- named(
    satisfy(function(x) grepl("^[ARNDCQEGHILKMFPSTWYV]+$", x)),
    "protein sequence"
  )

  fasta_line <- header %or% nucl_seq %or% prot_seq

  err <- tryCatch(
    reporter(fasta_line %then% eof())("12345"),
    error = function(e) e
  )

  expect_match(err$message, "Expected one of:")
  expect_match(err$message, "header line")
  expect_match(err$message, "nucleotide sequence")
  expect_match(err$message, "protein sequence")
})

test_that("'one_or_more' propagates expected information", {
  reset_LNR()
  # one_or_more requires at least one match
  r <- one_or_more(literal("A"))("B")
  expect_true(failed(r))
  expect_identical(marker_expected(r), "literal 'A'")
})

test_that("'match_n' propagates expected information", {
  reset_LNR()
  r <- match_n(2, literal("A"))(c("A", "B"))
  expect_true(failed(r))
  expect_identical(marker_expected(r), "literal 'A'")
})

test_that("'match_s' with custom stringparser maintains error reporting", {
  reset_LNR()
  get_numbers <- function(x) {
    m <- gregexpr("[[:digit:]]+", x)
    matches <- regmatches(x, m)[[1]]
    if (length(matches) == 0) {
      return(list())
    } else {
      return(as.numeric(matches))
    }
  }

  r <- match_s(get_numbers)("no numbers here")
  expect_true(failed(r))
  # match_s doesn't add expected by default, but the error still occurs
})

test_that("empty input produces expected information", {
  reset_LNR()
  r <- literal("A")(character(0))
  expect_true(failed(r))
  expect_identical(marker_expected(r), "literal 'A'")
})

test_that("'%using%' does not alter expected information", {
  reset_LNR()
  r <- (literal("A") %using% toupper)("B")
  expect_true(failed(r))
  expect_identical(marker_expected(r), "literal 'A'")
})

test_that("'%ret%' does not alter expected information", {
  reset_LNR()
  r <- (literal("A") %ret% "custom value")("B")
  expect_true(failed(r))
  expect_identical(marker_expected(r), "literal 'A'")
})

test_that("'exactly' propagates expected information", {
  reset_LNR()
  r <- exactly(2, literal("A"))(c("A", "A", "A"))
  expect_true(failed(r))
  # exactly fails when there are too many matches
})

test_that("'zero_or_one' propagates expected information on failure", {
  reset_LNR()
  r <- zero_or_one(literal("A"))(c("A", "A"))
  expect_true(failed(r))
  # Fails because there are two A's, not zero or one
})

test_that("EmptyLine and Spacer return expected messages on failure", {
  reset_LNR()
  r <- EmptyLine()("text content")
  expect_true(failed(r))
  expect_identical(marker_expected(r), "Emptyline")
  reset_LNR()
  r <- Spacer()("text content")
  expect_true(failed(r))
  expect_identical(marker_expected(r), "Spacer")
})

test_that("complex realistic parser produces helpful error messages", {
  reset_LNR()

  # Build a simple CSV-like parser
  number <- named(
    satisfy(function(x) grepl("^\\d+$", x)),
    "number"
  )
  text <- named(
    satisfy(function(x) grepl("^[a-zA-Z]+$", x)),
    "text"
  )

  csv_value <- number %or% text

  # Test with invalid input
  err <- tryCatch(
    reporter(csv_value %then% eof())("###"),
    error = function(e) e
  )

  expect_match(err$message, "line 1")
  expect_match(err$message, "Expected one of:")
  expect_match(err$message, "number")
  expect_match(err$message, "text")
})
