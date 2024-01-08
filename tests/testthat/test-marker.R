test_that("'new_marker' works", {
  expect_equal(new_marker(1), structure(list(), n = 1L, class = c("marker")))
  t <- new_marker(78)
  expect_identical(marker_val(t), 78L)
})

test_that("LNR works", {
  reset_LNR()
  expect_identical(LNR(), 1L)
  inc_LNR()
  expect_identical(LNR(), 2L)
  set_LNR(10)
  expect_identical(LNR(), 10L)
  reset_LNR()
  expect_identical(LNR(), 1L)
})

test_that("'finished' works", {
  expect_true(finished((literal("A") %then% eof())("A")))
  expect_false(finished((literal("A"))("A")))
  expect_false(finished((literal("A") %then% eof())(c("A","C"))))
})
