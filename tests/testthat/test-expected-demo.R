# Demonstration of Expected Element Error Reporting
# This file demonstrates the new functionality for showing expected elements
# in parser error messages, making debugging much easier.

test_that("Demo: Basic literal parser shows what was expected", {
  reset_LNR()

  # When a literal parser fails, it now tells you what it expected
  err <- tryCatch(
    reporter(literal("start") %then% eof())(c("wrong")),
    error = function(e) e
  )

  expect_match(err$message, "Expected: literal 'start'")
  expect_match(err$message, "line 1")
})

test_that("Demo: Multiple alternatives show all options", {
  reset_LNR()

  # When using %or%, the error shows all alternatives that could have worked
  command_parser <- literal("GET") %or%
    literal("POST") %or%
    literal("PUT") %or%
    literal("DELETE")

  err <- tryCatch(
    reporter(command_parser %then% eof())("PATCH"),
    error = function(e) e
  )

  expect_match(err$message, "Expected one of:")
  expect_match(err$message, "literal 'GET'")
  expect_match(err$message, "literal 'POST'")
  expect_match(err$message, "literal 'PUT'")
  expect_match(err$message, "literal 'DELETE'")
})

test_that("Demo: Named parsers provide semantic error messages", {
  reset_LNR()

  # Use named() to give semantic meaning to parsers
  DNA_base <- named(
    satisfy(function(x) x %in% c("A", "T", "G", "C")),
    "DNA base (A, T, G, or C)"
  )

  # Don't use eof() here - we want to see the DNA_base error, not eof error
  err <- tryCatch(
    reporter(DNA_base %then% eof())(c("X")),
    error = function(e) e
  )

  expect_match(err$message, "line 1")
  expect_match(err$message, "Expected: DNA base")
})

test_that("Demo: Complex parser with meaningful names", {
  reset_LNR()

  # Build a simple parser for structured data with meaningful names
  header <- named(
    satisfy(function(x) grepl("^#", x)),
    "header (line starting with #)"
  )

  number <- named(
    satisfy(function(x) grepl("^\\d+$", x)),
    "number"
  )

  text <- named(
    satisfy(function(x) grepl("^[a-zA-Z ]+$", x)),
    "text"
  )

  data_line <- number %or% text

  simple_file <- header %then% one_or_more(data_line) %then% eof()

  # Test the data_line parser directly to see its error message
  err <- tryCatch(
    reporter(data_line %then% eof())("!!!"),
    error = function(e) e
  )

  expect_match(err$message, "line 1")
  expect_match(err$message, "Expected one of:")
  expect_match(err$message, "number")
  expect_match(err$message, "text")
})

test_that("Demo: Fasta-like parser with clear error messages", {
  reset_LNR()

  # Realistic example: parsing sequence data
  fasta_header <- named(
    satisfy(function(x) grepl("^>\\w+", x)),
    "FASTA header (>sequence_name)"
  )

  nucleotide_line <- named(
    satisfy(function(x) grepl("^[GATC]+$", x)),
    "nucleotide sequence (only G, A, T, C)"
  )

  protein_line <- named(
    satisfy(function(x) grepl("^[ARNDCQEGHILKMFPSTWYV]+$", x)),
    "protein sequence"
  )

  sequence_line <- nucleotide_line %or% protein_line

  fasta_entry <- fasta_header %then% one_or_more(sequence_line)

  # Valid input works fine
  valid <- c(">seq1", "GATCGATC", "AAATTTGGG")
  result <- reporter(fasta_entry %then% eof())(valid)
  expect_true(is.list(result))

  # Invalid sequence gives clear error - test sequence_line directly
  err <- tryCatch(
    reporter(sequence_line %then% eof())("12345"),
    error = function(e) e
  )

  expect_match(err$message, "line 1")
  expect_match(err$message, "Expected one of:")
  expect_match(err$message, "nucleotide sequence")
  expect_match(err$message, "protein sequence")

  # Missing header gives clear error
  err2 <- tryCatch(
    reporter(fasta_entry %then% eof())(c("GATC", "AAATTT")),
    error = function(e) e
  )

  expect_match(err2$message, "line 1")
  expect_match(err2$message, "Expected: FASTA header")
})

test_that("Demo: Sequential parser errors point to exact failure", {
  reset_LNR()

  # Build a parser for a simple protocol
  version <- named(literal("v1.0"), "version 'v1.0'")
  separator <- literal("---")
  status_ok <- named(literal("OK"), "status 'OK'")
  status_err <- named(literal("ERROR"), "status 'ERROR'")
  status <- status_ok %or% status_err

  protocol <- version %then% separator %then% status %then% eof()

  # Error in first element
  err1 <- tryCatch(
    reporter(protocol)(c("v2.0", "---", "OK")),
    error = function(e) e
  )
  expect_match(err1$message, "line 1")
  expect_match(err1$message, "Expected: version 'v1.0'")

  # Error in middle element
  err2 <- tryCatch(
    reporter(protocol)(c("v1.0", "+++", "OK")),
    error = function(e) e
  )
  expect_match(err2$message, "line 2")
  expect_match(err2$message, "Expected: literal '---'")

  # Error in last element
  err3 <- tryCatch(
    reporter(protocol)(c("v1.0", "---", "PENDING")),
    error = function(e) e
  )
  expect_match(err3$message, "line 3")
  expect_match(err3$message, "Expected one of:")
  expect_match(err3$message, "status 'OK'")
  expect_match(err3$message, "status 'ERROR'")
})

test_that("Demo: Error messages help understand complex parser structure", {
  reset_LNR()

  # A parser for key-value configuration files
  comment <- named(
    satisfy(function(x) grepl("^#", x)),
    "comment line"
  )

  key_value <- named(
    satisfy(function(x) grepl("^\\w+\\s*=\\s*.+$", x)),
    "key=value pair"
  )

  config_line <- comment %or% EmptyLine() %or% key_value

  # This helps users understand what constitutes a valid line
  # Test config_line directly with invalid input
  err <- tryCatch(
    reporter(config_line %then% eof())("invalid line here"),
    error = function(e) e
  )

  expect_match(err$message, "line 1")
  # The error should mention what kinds of lines are acceptable
  expect_match(err$message, "Expected one of:")
})

test_that("Demo: Named wrappers replace underlying expectations", {
  reset_LNR()

  # Sometimes you want to hide implementation details
  # and show user-friendly names

  # Without named: shows technical details
  ip_octet_raw <- satisfy(function(x) {
    num <- suppressWarnings(as.numeric(x))
    !is.na(num) && num >= 0 && num <= 255
  })

  err_raw <- tryCatch(
    reporter(ip_octet_raw %then% eof())("999"),
    error = function(e) e
  )
  expect_match(err_raw$message, "matching input") # Generic message

  # With named: shows semantic meaning
  ip_octet <- named(
    satisfy(function(x) {
      num <- suppressWarnings(as.numeric(x))
      !is.na(num) && num >= 0 && num <= 255
    }),
    "IP octet (0-255)"
  )

  err_named <- tryCatch(
    reporter(ip_octet %then% eof())("999"),
    error = function(e) e
  )
  expect_match(err_named$message, "IP octet") # Clear message
})
