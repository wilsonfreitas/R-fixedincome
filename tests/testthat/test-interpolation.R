
terms <- c(1, 11, 26, 27, 28)
rates <- c(0.0719, 0.056, 0.0674, 0.0687, 0.07)

test_that("it should create an interpolation class", {
  interp <- interp_flatforward()
  expect_s4_class(interp, "Interpolation")
  expect_equal(as.character(interp), "flatforward")
  expect_equal(interp@.Data, "flatforward")
  expect_true(interp == "flatforward")
})

test_that("it should prepare an interpolation with a curve", {
  interp <- interp_linear()
  curve <- spotratecurve(rates, terms, "simple", "actual/365", "actual")
  prep_interp <- prepare_interpolation(interp, curve)
  expect_s4_class(prep_interp, "Interpolation")
})

test_that("it should interpolate many terms at once", {
  curve <- spotratecurve(rates, terms, "simple", "actual/365", "actual")
  interp <- interp_flatforward()
  interpolation(curve) <- interp
  x <- interpolate(curve@interpolation, 1:10)
  expect_type(x, "double")
  x <- interpolate(curve@interpolation, term(1:10, "days"))
  expect_type(x, "double")
})
