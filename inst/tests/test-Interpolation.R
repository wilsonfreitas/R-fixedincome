
context('Interpolation functions')

test_that("it should failt ", {
    expect_error( interp(0) )
})

test_that("it should interpolate FlatForwardly the interest rate curve", {
    curve <- SpotRateCurve(
        rates=seq(1, 0.98, length.out=10),
        terms=c(1, seq(21, by=21, length.out=9)),
        interp='FlatForward'
    )
    expect_equal( interp(curve, 1), 1 )
    expect_equal( interp(curve, 21), 0.9977778, tolerance=1e-6)
    expect_equal( interp(curve, 40), 0.9956666 )
})

test_that("it should interpolate Linearly the interest rate curve", {
    curve <- SpotRateCurve(
        rates=seq(1, 0.98, length.out=10),
        terms=c(1, seq(21, by=21, length.out=9)),
        interp='Linear'
    )
    expect_equal( interp(curve, 21), 0.9977778, tolerance=1e-6)
    expect_equal( interp(curve, 40), 0.9957672 )
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

test_that("it should interpolate the interest rate curve using Log-Linear interpolation", {
    curve <- SpotRateCurve(
        rates=seq(1, 0.98, length.out=10),
        terms=c(1, seq(21, by=21, length.out=9)),
        interp='LogLinear'
    )
    expect_equal( interp(curve, 21), 0.9977778, tolerance=1e-6 )
    expect_equal( interp(curve, 40), 0.995767, tolerance=1e-6 )
})
