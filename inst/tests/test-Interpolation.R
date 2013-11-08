
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
    # expect_equal(method(curve), 'flatforward')
    curve <- CurveInterpolation(curve, method='linear')
    # expect_equal(method(curve), 'linear')
})

test_that("it should interpolate FlatForwardly the interest rate curve using functions", {
    method <- flatforward(curve)
    expect_equal(method(21), 0.065400693)
    method <- do.call('flatforward', list(curve))
    expect_equal(method(21), 0.065400693)
})

test_that("it should create a CurveInterpolation with a single element", {
    curve <- SpotRateCurve(0.08, 1)
    curve <- CurveInterpolation(curve, 'flatforward')
    expect_equal(length(curve), 1)
    expect_equal(as.numeric(curve[1]), 0.08)
    expect_error(curve[2])
})

test_that("it should create a CurveInterpolation with a single element, add an element and interpolate", {
    curve <- SpotRateCurve(0.08, 1)
    curve <- CurveInterpolation(curve, 'flatforward')
    curve[3] <- 0.085
    expect_equal(length(curve), 2)
    expect_equal(as.numeric(curve[3]), 0.085)
    expect_equal(as.numeric(curve[2]), 0.08374783, tolerance=1e-6)
})

test_that("it should return a SpotRate for the given term using [[", {
    curve <- CurveInterpolation(curve, method='spline')
    expect_is( curve[[11]], 'SpotRate' )
    expect_is( curve[[21]], 'SpotRate' )
    expect_equal( rate(curve[[21]]), rates(curve[21]) )
    expect_equal( term(curve[[21]]), 21 )
    expect_equal( dib(curve[[21]]), dib(curve) )
    expect_equal( compounding(curve[[21]]), compounding(curve) )
})

test_that("it should interpolate FlatForwardly the interest rate curve", {
    curve <- CurveInterpolation(curve, method='flatforward')
    expect_is( curve[1], 'SpotRateCurve' )
    expect_is( curve[1], 'CurveInterpolation' )
    expect_equal( as.numeric(curve[1]), 0.0719 )
    expect_equal( as.numeric(curve[11]), 0.056 )
    expect_equal( as.numeric(curve[c(1, 11)]), c(0.0719, 0.056) )
    expect_equal( as.numeric(curve[21]), 0.065400693 )
    expect_equal( as.numeric(curve[c(14, 21)]), c(0.060220003, 0.065400693) )
    expect_equal( as.numeric(curve[c(1, 11, 14, 21)]), c(0.0719, 0.056, 0.060220003, 0.065400693) )
    expect_true( is.na(curve[30]) )
})

test_that("it should not extrapolate FlatForwardly the interest rate curve", {
    curve <- CurveInterpolation(curve, method='flatforward')
    expect_true( is.na(curve[30]) )
})

test_that("it should interpolate Linearly the interest rate curve", {
    curve <- CurveInterpolation(curve, method='linear')
    expect_equal( as.numeric(curve[1]), 0.0719 )
    expect_equal( as.numeric(curve[11]), 0.056 )
    expect_equal( as.numeric(curve[c(1, 11)]), c(0.0719, 0.056) )
    expect_equal( as.numeric(curve[21]), 0.0636 )
    expect_equal( as.numeric(curve[c(14, 21)]), c(0.05828, 0.0636) )
    expect_equal( as.numeric(curve[c(1, 11, 14, 21)]), c(0.0719, 0.056, 0.05828, 0.0636) )
})

test_that("it should not extrapolate Linearly the interest rate curve", {
    curve <- CurveInterpolation(curve, method='linear')
    expect_true( is.na(curve[30]) )
})

test_that("it should interpolate the interest rate curve using Log-Linear interpolation", {
    curve <- CurveInterpolation(curve, method='loglinear')
    expect_equal( as.numeric(curve[1]), 0.0719 )
    expect_equal( as.numeric(curve[11]), 0.056 )
    expect_equal( as.numeric(curve[c(1, 11)]), c(0.0719, 0.056) )
    expect_equal( as.numeric(curve[21]), 0.06336303 )
    expect_equal( as.numeric(curve[c(14, 21)]), c(0.058114218, 0.06336303) )
    expect_equal( as.numeric(curve[c(1, 11, 14, 21)]), c(0.0719, 0.056, 0.058114218, 0.06336303) )
})

test_that("it should not extrapolate Log-Linearly the interest rate curve", {
    curve <- CurveInterpolation(curve, method='loglinear')
    expect_true( is.na(curve[30]) )
})

test_that("it should add an element and interpolate FlatForwardly", {
    curve <- CurveInterpolation(curve)
    curve[32] <- 0.0643
    expect_equal( as.numeric(curve[32]), 0.0643 )
    expect_equal( as.numeric(curve[30]), 0.066956211 )
})

test_that("it should interpolate the interest rate curve using Natural Cubic Spline", {
    curve <- CurveInterpolation(curve, method='spline')
    expect_equal( as.numeric(curve[1]), 0.0719 )
    expect_equal( as.numeric(curve[11]), 0.056 )
    expect_equal( as.numeric(curve[c(1, 11)]), c(0.0719, 0.056) )
    expect_equal( as.numeric(curve[21]), 0.06085015, tolerance=1e-6 )
    expect_equal( as.numeric(curve[c(14, 21)]), c(0.05537827, 0.06085015), tolerance=1e-6 )
    expect_equal( as.numeric(curve[c(1, 11, 14, 21)]), c(0.0719, 0.056, 0.05537827, 0.06085015), tolerance=1e-6 )
})

test_that("it should interpolate the interest rate curve using Hermite Spline", {
    curve <- CurveInterpolation(curve, method='hermite')
    expect_equal( as.numeric(curve[1]), 0.0719 )
    expect_equal( as.numeric(curve[11]), 0.056 )
    expect_equal( as.numeric(curve[c(1, 11)]), c(0.0719, 0.056) )
    expect_equal( as.numeric(curve[21]), 0.06169444, tolerance=1e-6 )
    expect_equal( as.numeric(curve[c(14, 21)]), c(0.05589440, 0.06169444), tolerance=1e-6 )
    expect_equal( as.numeric(curve[c(1, 11, 14, 21)]), c(0.0719, 0.056, 0.05589440, 0.06169444), tolerance=1e-6 )
})

test_that("it should fail on creating the interest rate curve with Monotone Preserving Cubic Spline, y must be increasing or decreasing", {
    terms <- c(1, 11, 26, 27, 28)
    rates <- c(0.0719, 0.056, 0.0674, 0.0687, 0.07)
    curve <- SpotRateCurve(rates, terms)
    expect_error(CurveInterpolation(curve, method='monotone'))
})

terms <- c(1, 11, 26, 27, 28)
rates <- c(0.0519, 0.056, 0.0674, 0.0687, 0.07)
curve <- SpotRateCurve(rates, terms)

test_that("it should interpolate the interest rate curve using Monotone Preserving Cubic Spline", {
    curve <- CurveInterpolation(curve, method='monotone')
    expect_equal( as.numeric(curve[1]), 0.0519 )
    expect_equal( as.numeric(curve[11]), 0.056 )
    expect_equal( as.numeric(curve[c(1, 11)]), c(0.0519, 0.056) )
    expect_equal( as.numeric(curve[21]), 0.06209733, tolerance=1e-6 )
    expect_equal( as.numeric(curve[c(14, 21)]), c(0.05740306, 0.06209733), tolerance=1e-6 )
    expect_equal( as.numeric(curve[c(1, 11, 14, 21)]), c(0.0519, 0.056, 0.05740306, 0.06209733), tolerance=1e-6 )
})
