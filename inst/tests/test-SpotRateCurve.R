
context('spotratecurve class')

terms <- c(1, 11, 26, 27, 28)
rates <- c(0.0719, 0.056, 0.0674, 0.0687, 0.07)


test_that("it should create an interest rate curve", {
	spr <- as.spotrate(rates, simpleCompounding(), as.daycount('actual/365'))
	curve <- as.spotratecurve(terms, spr)
	expect_is(curve, "spotratecurve")
	expect_is(curve, "spotrate")
})

test_that("it should check if terms and rates have the same length", {
	spr <- as.spotrate(rates, simpleCompounding(), as.daycount('actual/365'))
	expect_error(as.spotratecurve(terms[-1], spr), "rates and terms must have the same length.")
})

test_that("it should check if terms are unique", {
	spr <- as.spotrate(rates, simpleCompounding(), as.daycount('actual/365'))
	expect_error(as.spotratecurve(rep(1, length(spr)), spr), "terms cannot have duplicated elements.")
})

test_that("it should check if terms are unique", {
	spr <- as.spotrate(rates, simpleCompounding(), as.daycount('actual/365'))
	expect_error(as.spotratecurve(-terms, spr), "all terms must be strictly positive.")
})

test_that("it should check if rates is an instance of spotrate", {
	spr <- as.spotrate(rates, simpleCompounding(), as.daycount('actual/365'))
	expect_error(as.spotratecurve(terms, rates), "rates must be an instance of spotrate.")
})

test_that("it should check if terms is an ascending ordered collection", {
	spr <- as.spotrate(rates, simpleCompounding(), as.daycount('actual/365'))
	expect_error(as.spotratecurve(sample(terms), spr), "terms must be ordered ascending.")
})

test_that("it should access rates attribute", {
	spr <- as.spotrate(rates, simpleCompounding(), as.daycount('actual/365'))
	curve <- as.spotratecurve(terms, spr)
    expect_equal(rates(curve), rates)
})

test_that("it should access terms attribute", {
	spr <- as.spotrate(rates, simpleCompounding(), as.daycount('actual/365'))
	curve <- as.spotratecurve(terms, spr)
	expect_equal(terms(curve), terms)
})

test_that("it should return the curve's length", {
	spr <- as.spotrate(rates, simpleCompounding(), as.daycount('actual/365'))
	curve <- as.spotratecurve(terms, spr)
	expect_equal(length(curve), 5)
})

test_that("it should index the elements", {
	spr <- as.spotrate(rates, simpleCompounding(), as.daycount('actual/365'))
	curve <- as.spotratecurve(terms, spr)
	expect_equal( curve[1], 0.0719 )
	# expect_equal( curve[11], 0.056 )
	# expect_equal( curve[c(1, 11)], c(0.0719, 0.056) )
	# expect_is( curve[-c(1,11)], 'SpotRateCurve' ) # remove elements
	# expect_error( curve[21] ) # interpolate
})

# test_that("it should execute indexing operations", {
#     expect_equal( which(curve == 0.0719), 1 )
#     expect_equal( which(curve < 0.0719), c(2, 3, 4, 5) )
# })
#
# test_that("it should return a new curve with remaining elements", {
#     expect_equal( length(curve[-11]), length(curve)-1 )
#     expect_equal( length(curve[-c(1,11)]), length(curve)-2 )
#     expect_is( curve[-c(1,11)], 'SpotRateCurve' )
#     expect_error( curve[-21] )
# })
#
# test_that("it should return a SpotRate for the given term", {
#     expect_is( getSpotRate(curve, 11), 'SpotRate' )
#     expect_equal( getSpotRate(curve, 11), curve[[11]] )
#     expect_equal( term(getSpotRate(curve, 11)), 11 )
#     expect_equal( dib(getSpotRate(curve, 11)), dib(curve) )
#     expect_equal( compounding(getSpotRate(curve, 11)), compounding(curve) )
#     expect_error( getSpotRate(curve, 21) )
# })
#
# test_that("it should return a SpotRate for the given term using [[", {
#     expect_is( curve[[11]], 'SpotRate' )
#     expect_error( curve[[21]] )
# })
#
# test_that("it should compute forward rates", {
#     expect_error( forward.rate(curve, 1, 1) )
#     expect_equal( forward.rate(curve, 1, 11), SpotRate(0.05442303, 10) )
#     expect_equal( forward.rate(curve, 1, forward.term=10), SpotRate(0.05442303, 10) )
#     expect_error( forward.rate(curve, 1, forward.term=11) )
# })
#
# test_that('it should append a SpotRate to a SpotRateCurve', {
#     curve[32] <- 0.0643
#     expect_equal(length(curve), 6)
#     expect_equal(terms(curve), c(1, 11, 26, 27, 28, 32))
#     expect_equal(rates(curve), c(0.0719, 0.056, 0.0674, 0.0687, 0.07, 0.0643))
#
#     curve[3] <- 0.07
#     expect_equal(length(curve), 7)
#     expect_equal(terms(curve), c(1, 3, 11, 26, 27, 28, 32))
#     expect_equal(rates(curve), c(0.0719, 0.07, 0.056, 0.0674, 0.0687, 0.07, 0.0643))
#
#     curve[3] <- 0.06
#     expect_equal(length(curve), 7)
#     expect_equal(terms(curve), c(1, 3, 11, 26, 27, 28, 32))
#     expect_equal(rates(curve), c(0.0719, 0.06, 0.056, 0.0674, 0.0687, 0.07, 0.0643))
#
#     curve[c(3,7,6)] <- c(0.059, 0.058, 0.057)
#     expect_equal(length(curve), 9)
#     expect_equal(terms(curve), c(1, 3, 6, 7, 11, 26, 27, 28, 32))
#     expect_equal(rates(curve), c(0.0719, 0.059, 0.057, 0.058, 0.056, 0.0674, 0.0687, 0.07, 0.0643))
# })
#
# test_that('it should find the neighbors for a given term', {
#     expect_equal(neighbors(curve, 11), c(11, 11))
#     expect_equal(neighbors(curve, 21), c(11, 26))
# })
#
# test_that('it should define a datum and a Calendar to a curve', {
# 	terms <- c(1, 11, 26, 27, 28)
# 	rates <- c(0.0719, 0.056, 0.0674, 0.0687, 0.07)
# 	curve <- SpotRateCurve(rates, terms, datum='2013-10-28')
# 	expect_equal(datum(curve), as.Date('2013-10-28'))
# })
#
# test_that('it should define a name to a curve', {
# 	terms <- c(1, 11, 26, 27, 28)
# 	rates <- c(0.0719, 0.056, 0.0674, 0.0687, 0.07)
# 	curve <- SpotRateCurve(rates, terms, name='CURVE')
# 	expect_equal(name(curve), 'CURVE')
# })
#
# test_that('it should return the curve\'s head', {
# 	terms <- c(1, 11, 26, 27, 28, 30)
# 	rates <- c(0.0719, 0.056, 0.0674, 0.0687, 0.07, 0.07)
# 	curve <- SpotRateCurve(rates, terms)
# 	expect_is(head(curve), 'SpotRateCurve')
# 	expect_equal(length(head(curve)), 6)
# 	expect_equal(head(curve), curve)
# 	expect_error(head(curve, 10))
# })
#
# test_that('it should return the curve\'s tail', {
# 	terms <- c(1, 11, 26, 27, 28, 30)
# 	rates <- c(0.0719, 0.056, 0.0674, 0.0687, 0.07, 0.07)
# 	curve <- SpotRateCurve(rates, terms)
# 	expect_is(tail(curve), 'SpotRateCurve')
# 	expect_equal(length(tail(curve, 3)), 3)
# 	expect_equal(tail(curve), curve)
# 	expect_error(tail(curve, 10))
# })