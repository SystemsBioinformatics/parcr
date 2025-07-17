test_that("'lineparser' creates correct line parsers", {
  parse_header <- lineparser("^>(\\w+)")
  parse_header_no_output <- lineparser("^>\\w+")
  expect_equal(parse_header(">good_header"), "good_header")
  expect_equal(parse_header("> bad_header"), list())
  expect_silent(parse_header_no_output(">good_header"))
  expect_equal(parse_header_no_output("> bad_header"), list())
})
