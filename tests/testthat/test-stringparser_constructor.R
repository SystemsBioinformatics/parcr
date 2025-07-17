test_that("'stringparser' creates parser with zero or one capture groups", {
  parse_header <- stringparser("^>(\\w+)")
  parse_header_no_output <- stringparser("^>\\w+")
  expect_equal(parse_header(">good_header"), "good_header")
  expect_equal(parse_header("> bad_header"), list())
  expect_silent(parse_header_no_output(">good_header"))
  expect_equal(parse_header_no_output("> bad_header"), list())
})

test_that("'stringparser' creates parser with multiple capture groups and arbitrary output", {
  parse_key_value <- stringparser("(\\w+):\\s?(\\w+)")
  parse_key_value_df <- stringparser("(\\w+):\\s?(\\w+)", "data.frame(key = m[1], value = m[2])")
  expect_equal(parse_key_value("key1 value1"), list())
  expect_equal(parse_key_value("key1: value1"), c("key1", "value1"))
  expect_equal(parse_key_value_df("key1: value1"), data.frame(key = 'key1', value = 'value1'))
})
