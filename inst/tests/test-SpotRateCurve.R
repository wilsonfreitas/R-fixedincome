
context('SpotRateCurve calculations')

terms <- c(1, 11, 26, 27, 28)
rates <- c(0.0719, 0.056, 0.0674, 0.0687, 0.07)
curve <- SpotRateCurve(rates, terms)

test_that("it should create an interest rate curve", {
    curve <- SpotRateCurve(rates, terms, dib=360, compounding='compounded')
    expect_is( curve, "SpotRateCurve" )
})

test_that("it should check if terms and rates have the same length", {
    expect_error( SpotRateCurve(numeric(10), 1:11) )
    SpotRateCurve(numeric(10), 1:10)
})

test_that("it should check if terms has unique elements", {
    expect_error( SpotRateCurve(numeric(10), numeric(10)) )
    SpotRateCurve(numeric(10), 1:10)
})

test_that("it should check if terms is an ascending ordered collection", {
    expect_error( SpotRateCurve(numeric(10), sample(1:10)) )
    SpotRateCurve(numeric(10), 1:10)
})

test_that("it should access rates attribute", {
    expect_equal( rates(curve), rates )
})

test_that("it should access terms attribute", {
    expect_equal( terms(curve), terms )
})

test_that("it should return the interest rate curve length", {
    expect_equal( length(curve), 5 )
})

test_that("it should access the elements by its indexes", {
    expect_equal( curve[1], 0.0719 )
    expect_equal( curve[11], 0.056 )
    expect_equal( curve[c(1, 11)], c(0.0719, 0.056) )
    expect_error( curve[21] )
})

test_that("it should return a new curve with remaining elements", {
    expect_equal( curve[-1], length(curve)-1 )
})

test_that("it should return a SpotRate for the given term", {
    expect_is( getSpotRate(curve, 11), 'SpotRate' )
    expect_equal( rate(getSpotRate(curve, 11)), curve[11] )
    expect_equal( term(getSpotRate(curve, 11)), 11 )
    expect_equal( dib(getSpotRate(curve, 11)), dib(curve) )
    expect_equal( compounding(getSpotRate(curve, 11)), compounding(curve) )
    expect_error( getSpotRate(curve, 21) )
})

test_that("it should compute forward rates", {
    expect_error( forward.rate(curve, 1, 1) )
    expect_equal( forward.rate(curve, 1, 11), SpotRate(0.05442303, 10) )
    expect_equal( forward.rate(curve, 1, forward.term=10), SpotRate(0.05442303, 10) )
    expect_error( forward.rate(curve, 1, forward.term=11) )
})

test_that('it should append a SpotRate to a SpotRateCurve', {
    curve[32] <- 0.0643
    expect_equal(length(curve), 6)
    expect_equal(terms(curve), c(1, 11, 26, 27, 28, 32))
    expect_equal(rates(curve), c(0.0719, 0.056, 0.0674, 0.0687, 0.07, 0.0643))

    curve[3] <- 0.07
    expect_equal(length(curve), 7)
    expect_equal(terms(curve), c(1, 3, 11, 26, 27, 28, 32))
    expect_equal(rates(curve), c(0.0719, 0.07, 0.056, 0.0674, 0.0687, 0.07, 0.0643))

    curve[3] <- 0.06
    expect_equal(length(curve), 7)
    expect_equal(terms(curve), c(1, 3, 11, 26, 27, 28, 32))
    expect_equal(rates(curve), c(0.0719, 0.06, 0.056, 0.0674, 0.0687, 0.07, 0.0643))

    curve[c(3,7,6)] <- c(0.059, 0.058, 0.057)
    expect_equal(length(curve), 9)
    expect_equal(terms(curve), c(1, 3, 6, 7, 11, 26, 27, 28, 32))
    expect_equal(rates(curve), c(0.0719, 0.059, 0.057, 0.058, 0.056, 0.0674, 0.0687, 0.07, 0.0643))
})

test_that('it should find the neighbors for a given term', {
    expect_equal(neighbors(curve, 11), c(11, 11))
    expect_equal(neighbors(curve, 21), c(11, 26))
})