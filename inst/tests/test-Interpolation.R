
context('Interpolation functions')

test_that("it should interpolate FlatForwardly the interest rate curve ", {
    curve <- SpotRateCurve(rates=seq(1, 0.98, length.out=10),
                    terms=c(1, seq(21, by=21, length.out=9)))
    expect_equal( interp.FlatForward(curve, 1), 1 )
    expect_equal( interp.FlatForward(curve, 21), 0.9977778, tolerance=1e-6)
    expect_equal( interp.FlatForward(curve, 40), 0.9956666 )
    expect_error( interp.FlatForward(0) )
})

