
context('Interpolation functions')

terms <- c(1, 11, 26, 27, 28)
rates <- c(0.0719, 0.056, 0.0674, 0.0687, 0.07)
curve <- SpotRateCurve(rates, terms)

test_that("it should create a interolation class", {
    curve <- CurveInterpolation(curve, method='flatforward')
    expect_true(inherits(curve, c('CurveInterpolation', 'SpotRateCurve')))
})

test_that("it should set the method of a interpolation class", {
    curve <- CurveInterpolation(curve)
    expect_equal(method(curve), 'flatforward')
    curve <- CurveInterpolation(curve, method='linear')
    expect_equal(method(curve), 'linear')
})

test_that("it should interpolate FlatForwardly the interest rate curve using functions", {
    method <- interpolationMethods[['flatforward']]
    interp.FUN <- method$prepare(curve)
    expect_equal(method$interp(curve, 21, interp.FUN), 0.065400693)
})

test_that("it should interpolate FlatForwardly the interest rate curve", {
    curve <- CurveInterpolation(curve, method='flatforward')
    expect_equal( curve[1], 0.0719 )
    expect_equal( curve[11], 0.056 )
    expect_equal( curve[c(1, 11)], c(0.0719, 0.056) )
    expect_equal( curve[21], 0.065400693 )
    expect_equal( curve[c(14, 21)], c(0.060220003, 0.065400693) )
    expect_equal( curve[c(1, 11, 14, 21)], c(0.0719, 0.056, 0.060220003, 0.065400693) )
    expect_true( is.na(curve[30]) )
})

test_that("it should not extrapolate FlatForwardly the interest rate curve", {
    curve <- CurveInterpolation(curve, method='flatforward')
    expect_true( is.na(curve[30]) )
})

test_that("it should interpolate Linearly the interest rate curve", {
    curve <- CurveInterpolation(curve, method='linear')
    expect_equal( curve[1], 0.0719 )
    expect_equal( curve[11], 0.056 )
    expect_equal( curve[c(1, 11)], c(0.0719, 0.056) )
    expect_equal( curve[21], 0.0636 )
    expect_equal( curve[c(14, 21)], c(0.05828, 0.0636) )
    expect_equal( curve[c(1, 11, 14, 21)], c(0.0719, 0.056, 0.05828, 0.0636) )
})

test_that("it should not extrapolate Linearly the interest rate curve", {
    curve <- CurveInterpolation(curve, method='linear')
    expect_true( is.na(curve[30]) )
})

test_that("it should interpolate the interest rate curve using Log-Linear interpolation", {
    curve <- CurveInterpolation(curve, method='loglinear')
    expect_equal( curve[1], 0.0719 )
    expect_equal( curve[11], 0.056 )
    expect_equal( curve[c(1, 11)], c(0.0719, 0.056) )
    expect_equal( curve[21], 0.06336303 )
    expect_equal( curve[c(14, 21)], c(0.058114218, 0.06336303) )
    expect_equal( curve[c(1, 11, 14, 21)], c(0.0719, 0.056, 0.058114218, 0.06336303) )
})

test_that("it should not extrapolate Log-Linearly the interest rate curve", {
    curve <- CurveInterpolation(curve, method='loglinear')
    expect_true( is.na(curve[30]) )
})

test_that("it should add an element and interpolate FlatForwardly", {
    curve <- CurveInterpolation(curve)
    curve[32] <- 0.0643
    expect_equal( curve[32], 0.0643 )
    expect_equal( curve[30], 0.066956211 )
})

test_that("it should interpolate the interest rate curve using Natural Cubic Spline", {
    curve <- SpotRateCurve(
        rates=seq(1, 0.98, length.out=10),
        terms=c(1, seq(21, by=21, length.out=9)),
        interp='Spline'
    )
    expect_equal( interp(curve, 21), 0.9977778, tolerance=1e-6 )
    expect_equal( interp(curve, 40), 0.9957657, tolerance=1e-6 )
})

test_that("it should interpolate the interest rate curve using Hermite Spline", {
    curve <- SpotRateCurve(
        rates=seq(1, 0.98, length.out=10),
        terms=c(1, seq(21, by=21, length.out=9)),
        interp='Hermite'
    )
    expect_equal( interp(curve, 21), 0.9977778, tolerance=1e-6 )
    expect_equal( interp(curve, 40), 0.9957667, tolerance=1e-6 )
})

test_that("it should interpolate the interest rate curve using Monotone Preserving Cubic Spline", {
    curve <- SpotRateCurve(
        rates=seq(1, 0.98, length.out=10),
        terms=c(1, seq(21, by=21, length.out=9)),
        interp='Monotone'
    )
    expect_equal( interp(curve, 21), 0.9977778, tolerance=1e-6 )
    expect_equal( interp(curve, 40), 0.9957667, tolerance=1e-6 )
})

