test_that("'new_tracker' works", {
  expect_equal(new_tracker(1), structure(list(), n = 1L, class = c("tracker")))
  t <- new_tracker(78)
  expect_identical(tracker_val(t), 78L)
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
