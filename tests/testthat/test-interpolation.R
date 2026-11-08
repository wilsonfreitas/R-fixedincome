
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

# B3 curve manual: exponential interpolation of the capitalization factor in business days
b3_flatforward <- function(t, vt, vr) {
  i <- findInterval(t, vt)
  f1 <- (1 + vr[i])^(vt[i] / 252)
  f2 <- (1 + vr[i + 1])^(vt[i + 1] / 252)
  (f1 * (f2 / f1)^((t - vt[i]) / (vt[i + 1] - vt[i])))^(252 / t) - 1
}

test_that("flatforward matches the B3 manual formula (#31)", {
  vt <- c(1, 21, 43, 65, 292, 545, 1046)
  vr <- c(0.1365, 0.1366, 0.1368, 0.1371, 0.1290, 0.1170, 0.1150)
  curve <- spotratecurve(vr, vt, "discrete", "business/252", "Brazil/ANBIMA")
  interpolation(curve) <- interp_flatforward()
  t <- c(2, 10, 30, 50, 100, 400, 1000)
  expect_equal(as.numeric(curve[[t]]), b3_flatforward(t, vt, vr), tolerance = 1e-12)
})

test_that("flatforward reproduces B3 PRE curve within rounding (#31)", {
  # B3 publishes rates rounded to 1bp, so vertex inputs carry up to 0.5bp error
  yc <- readRDS(test_path("fixtures", "b3-pre-2022-10-31.rds"))
  v <- yc[yc$vertex, ]
  curve <- spotratecurve(v$r_252, v$biz_days, "discrete", "business/252", "Brazil/ANBIMA")
  interpolation(curve) <- interp_flatforward()
  pts <- yc[!yc$vertex & yc$biz_days < max(v$biz_days), ]
  err_bp <- abs(as.numeric(curve[[pts$biz_days]]) - pts$r_252) * 1e4
  expect_lt(max(err_bp), 1)
})
