
context('spotrate class')

test_that("it should create a spotrate", {
	spr <- spotrate(0.06, 'simple', 'actual/365')
	expect_true(spr == 0.06)
	expect_is(spr, 'spotrate')
})

test_that("it should coerce a spotrate to string", {
	spr <- spotrate(0.06, 'simple', 'actual/365')
	expect_true(as.character(spr) == '0.06 simple actual/365')
	spr <- spotrate(0.06, 'continuous', 'actual/365', Calendar(name='actual'))
	expect_true(as.character(spr) == '0.06 continuous actual/365 actual')
})

test_that("it should parse a string to build a spotrate", {
	spr <- as.spotrate('0.06 simple actual/365')
	expect_is(spr, 'spotrate')
	expect_true(spr == 0.06)
	expect_true(compounding(spr) == 'simple')
	expect_true(daycount(spr) == 'actual/365')
	expect_true(is.null(calendar(spr)))
	expect_true(as.character(spr) == '0.06 simple actual/365')
})

test_that("it should compute a compounding factor for fixed periods", {
	spr <- spotrate(0.06, 'simple', 'actual/365')
	expect_equal(compound(spr, 10), 1.001643836)
	expect_equal(compound(spr, 1, 'years'), 1.06)
	expect_equal(compound(spr, as.term(10, 'days')), 1.001643836)
	expect_equal(compound(spr, as.term('10 days')), 1.001643836)
	expect_equal(compound(spr, '1 months'), 1.005)
	expect_equal(compound(spr, '1 years'), 1.06)
})

test_that("it should raise an error: Invalid term", {
	spr <- spotrate(0.06, 'simple', 'actual/365')
	expect_error(compound(spr, '1 month'), 'Invalid term: 1 month')
})

test_that("it should compute a compounding factor between dates", {
	library(bizdays)
	spr <- spotrate(0.06, 'continuous', 'actual/365', Calendar(name='actual'))
	expect_equal(compound(spr, from='2013-01-01', to='2013-01-02'), 1.000164397)
})

test_that("it should raise an error for trying to compound factor between dates without calendar", {
	spr <- spotrate(0.06, 'continuous', 'actual/365')
	expect_error(compound(spr, from='2013-01-01', to='2013-01-02'), 'Missing calendar')
})

test_that("it should create a spotrate vectorized", {
	spr <- spotrate(c(0.06, 0.07, 0.08), 'simple', 'actual/365')
	expect_true(all(spr == c(0.06, 0.07, 0.08)))
})

test_that("it should create a spotrate vectorized with NAs", {
	spr <- spotrate(c(0.06, NA, 0.08), 'simple', 'actual/365')
	expect_true(all(spr == c(0.06, NA, 0.08), na.rm=TRUE))
})

test_that("it should coerce a spotrate to string (vectorized)", {
	spr <- spotrate(c(0.06, 0.07, 0.08), 'simple', 'actual/365')
	expect_true(as.character(spr) == 'simple actual/365\n0.06 0.07 0.08')
	spr <- spotrate(c(0.06, NA, 0.08), 'continuous', 'actual/365', Calendar(name='actual'))
	expect_true(as.character(spr) == 'continuous actual/365 actual\n0.06   NA 0.08')
})

test_that("it should compute a compounding factor for fixed periods (vectorized)", {
	spr <- spotrate(c(0.06, 0.07, 0.08), 'simple', 'actual/365')
	expect_equal(compound(spr, 10), c(1.001643836, 1.001917808, 1.002191781))
	expect_equal(compound(spr, c(10, 10, 10)), c(1.001643836, 1.001917808, 1.002191781))
	spr <- as.spotrate(0.06, 'simple', 'actual/365')
	expect_equal(compound(spr, c(10, 10, 10)), c(1.001643836, 1.001643836, 1.001643836))
})

test_that("it should discount a spot rate", {
	spr <- spotrate(0.06, 'simple', 'actual/365')
	expect_equal(discount(spr, 10), 0.9983588618)
	library(bizdays)
	spr <- spotrate(0.06, 'continuous', 'actual/365', Calendar(name='actual'))
	expect_equal(discount(spr, from='2013-01-01', to='2013-01-02'), 0.99983563)
})

test_that("it should test zero term", {
	spr <- spotrate(0.06, 'simple', 'actual/365')
	expect_equal(discount(spr, 0), 1)
	expect_equal(discount(spr, 0, 'years'), 1)
})

test_that("it should convert a spotrate to another spotrate", {
	spr_s <- spotrate(0.06, 'simple', 'actual/365')
	spr_d <- as.spotrate(spr_s, as.term('1 years'), 'discrete')
	expect_equal(as.numeric(spr_d), 0.06)
	# simple change
	spr_d <- as.spotrate(spr_s, as.term('1 months'), 'discrete')
	expect_equal(as.numeric(spr_d), 0.06167781186)
	# vectorized
	spr_s <- spotrate(c(0.06, 0.07, 0.08), 'simple', 'actual/365')
	spr_d <- as.spotrate(spr_s, as.term('1 months'), 'discrete')
	expect_equal(as.numeric(spr_d), c(0.06167781186, 0.07229008086, 0.08299950681))
	# change daycount/calendar
	# spr_s <- as.spotrate(10.68/100, discreteCompounding(), as.daycount('actual/360'))
	# spr_d <- as.spotrate(spr_s, as.term('1 days'), daycount=as.daycount('business/252'))
	# expect_equal(as.numeric(spr_d), 10.80/100)
})

test_that('it should convert to list', {
  spr <- as.spotrate(c(0.06, 0.07, 0.08), 'simple', 'actual/365')
  l <- as.list(spr)
})

