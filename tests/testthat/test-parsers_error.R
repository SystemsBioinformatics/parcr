# ---- Helper utilities ---------------------------------------------------------


# ---- Tests -------------------------------------------------------------------

p1 <- literal("a") %then% literal("b")
p2 <- p1 %then% eof()
p3 <- literal("l1") %then% literal("l2") %then% literal("l3") %then% literal("l4") %then% eof()
p4 <- one_or_more(literal("l")) %then% eof()

test_that("Reporter succeeds without error or warning", {
  p <- reporter(p2)
  expect_equal(p(c("a", "b")), list("a", "b"))
})

test_that("Reporter warns that input is not fully consumed", {
  p <- reporter(p1)
  expect_warning(
    p(c("a", "b")),
    "The parser did not completely consume the input"
  )
})

test_that("reporter errors with correct line number (simple case)", {
  p <- reporter(p3)
  err <- tryCatch(
    p(c("l1", "l2", "l2", "l4")),
    error = function(e) e
  )
  expect_true(inherits(err, "error_parser"))
  expect_match(err$message, "Parser failed on line 3")
})

test_that("reporter includes surrounding context in error message", {
  p <- reporter(p3, context_size = 3)
  err <- tryCatch(
    p(c("l1", "l2", "l2", "l4")),
    error = function(e) e
  )
  # Context should start at line 4 (5 - floor(3/2) = 4) and end at line 6
  expect_match(err$message, "2 \\|")
  expect_match(err$message, "3 \\| >>")
  expect_match(err$message, "4 \\|")
})

test_that("reporter correctly prints large line numbers", {
  p <- reporter(p4, context_size = 3)
  err <- tryCatch(
    p(c(rep("l", 99), "a", "b", "c")),
    error = function(e) e
  )
  # Context should start at line 4 (5 - floor(3/2) = 4) and end at line 6
  expect_match(err$message, " 99 \\|")
  expect_match(err$message, "100 \\| >>")
  expect_match(err$message, "101 \\|")
})

test_that("parser_error_context is correctly calculated", {
  expect_equal(parser_error_context(1, LETTERS[1:6], 5), list(linenrs = seq(1, 5), context = LETTERS[1:5], failed_line = 1))
})
