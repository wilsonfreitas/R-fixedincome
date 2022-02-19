test_that("it should shift vectors", {
  expect_equal(shift(1:3), c(NA, 1, 2))
})
