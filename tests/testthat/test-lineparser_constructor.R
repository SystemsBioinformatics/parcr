test_that("'lineparser' creates parser with zero or one capture groups", {
  parse_header <- lineparser("^>(\\w+)")
  parse_header_no_output <- lineparser("^>\\w+")
  expect_equal(parse_header(">good_header"), "good_header")
  expect_equal(parse_header("> bad_header"), list())
  expect_silent(parse_header_no_output(">good_header"))
  expect_equal(parse_header_no_output("> bad_header"), list())
})

test_that("'lineparser' creates parser with multiple capture groups and arbitrary output", {
  parse_key_value <- lineparser("(\\w+):\\s?(\\w+)")
  parse_key_value_df <- lineparser("(\\w+):\\s?(\\w+)", "data.frame(key = m[1], value = m[2])")
  expect_equal(parse_key_value("key1 value1"), list())
  expect_equal(parse_key_value("key1: value1"), c("key1", "value1"))
  expect_equal(parse_key_value_df("key1: value1"), data.frame(key = 'key1', value = 'value1'))
})
