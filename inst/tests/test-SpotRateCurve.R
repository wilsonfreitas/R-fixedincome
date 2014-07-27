
context('spotratecurve class')

terms <- c(1, 11, 26, 27, 28)
rates <- c(0.0719, 0.056, 0.0674, 0.0687, 0.07)


test_that("it should create an interest rate curve", {
	spr <- as.spotrate(rates, simpleCompounding(), as.daycount('actual/365'))
	curve <- as.spotratecurve(terms, spr)
	expect_is(curve, "spotratecurve")
	expect_is(curve, "spotrate")
	expect_true(attr(curve, 'units') == "days")
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
	expect_equal(terms(curve, units='days'), terms)
	expect_equal(terms(curve, units='years'), terms/365)
})

test_that("it should return the curve's length", {
	spr <- as.spotrate(rates, simpleCompounding(), as.daycount('actual/365'))
	curve <- as.spotratecurve(terms, spr)
	expect_equal(length(curve), 5)
})

test_that("it should check if indexed the elements is spotrate", {
	spr <- as.spotrate(rates, simpleCompounding(), as.daycount('actual/365'))
	curve <- as.spotratecurve(terms, spr, interp=linear)
	expect_is(curve[1], 'spotrate')
})

test_that("it should index the elements", {
	spr <- as.spotrate(rates, simpleCompounding(), as.daycount('actual/365'))
	curve <- as.spotratecurve(terms, spr, interp=linear)
	expect_true(curve[1] == 0.0719)
	expect_true(curve[11] == 0.056)
	expect_true(all(curve[c(1, 11)] == c(0.0719, 0.056)))
})

# test_that("it should remove elements", {
	# spr <- as.spotrate(rates, simpleCompounding(), as.daycount('actual/365'))
	# curve <- as.spotratecurve(terms, spr)
	# expect_is( curve[-c(1,11)], 'SpotRateCurve' ) # remove elements
# })

test_that("it should interpolate", {
	spr <- as.spotrate(rates, simpleCompounding(), as.daycount('actual/365'))
	curve <- as.spotratecurve(terms, spr, interp=linear)
	expect_true(curve[21] == 0.0636)
	expect_true(all(curve[c(11, 21, 26)] == c(0.0560, 0.0636, 0.0674)))
})

test_that("it should create a curve using dates", {
	library(bizdays)
	spr <- as.spotrate(rates, simpleCompounding(), as.daycount('actual/365'), Calendar(name='Actual'))
	curve <- as.spotratecurve(terms+Sys.Date(), spr, refdate=Sys.Date(), interp=linear)
	expect_is(curve, "spotratecurve")
	expect_is(terms(curve), "Date")
	expect_true(all(terms(curve) == terms+Sys.Date()))
	expect_true(all(terms(curve, as.x=TRUE) == terms))
})

test_that("it should interpolate a curve using dates", {
	library(bizdays)
	spr <- as.spotrate(rates, simpleCompounding(), as.daycount('actual/365'), Calendar(name='Actual'))
	curve <- as.spotratecurve(terms+as.Date('2014-07-01'), spr, refdate=as.Date('2014-07-01'))
	expect_true(curve['2014-07-02'] == 0.0719)
	expect_equal(rates(curve[c('2014-07-02', '2014-07-03')]), c(0.0719, 0.07031))
	expect_true(curve['2014-07-04'] == curve[3])
})

test_that("it should replace a curve element using dates", {
	library(bizdays)
	spr <- as.spotrate(rates, simpleCompounding(), as.daycount('actual/365'), Calendar(name='Actual'))
	curve <- as.spotratecurve(terms+as.Date('2014-07-01'), spr, refdate=as.Date('2014-07-01'))
	curve['2014-07-02'] <- 1
	expect_true(curve['2014-07-02'] == 1)
	curve['2014-07-03'] <- 1
	expect_true(curve[2] == 1)
	curve[c('2014-07-04', '2014-07-05')] <- 1
	expect_true(all(curve[c(3, 4)] == 1))
})

test_that("it should create a curve using numeric and dates", {
	library(bizdays)
	cal <- Calendar(name='Actual')
	spr <- as.spotrate(rates, simpleCompounding(), as.daycount('actual/365'), cal)
	curve <- as.spotratecurve(terms, spr, refdate=Sys.Date(), interp=linear)
	expect_is(curve, "spotratecurve")
	expect_is(terms(curve), "Date")
	.terms <- sapply(terms, function(x) add.bizdays(Sys.Date(), x, cal))
	expect_true(all(terms(curve) == .terms))
	expect_true(all(terms(curve, as.x=TRUE) == terms))
})

test_that("it should interpolate a curve using dates and numbers", {
	library(bizdays)
	cal <- Calendar(name='Actual')
	spr <- as.spotrate(rates, 'simple', 'actual/365', cal)
	curve <- as.spotratecurve(terms, spr, refdate='2014-07-01', interp=linear)
	expect_true(curve['2014-07-02'] == curve[1])
	expect_equal(rates(curve[c('2014-07-02', '2014-07-03')]), rates(curve[c(1, 2)]))
	expect_true(curve['2014-07-04'] == curve[3])
})

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