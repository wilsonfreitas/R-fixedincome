
context('daycount class')

test_that("it should create a daycount object", {
	dc <- as.daycount('actual/360')
	expect_true(as.character(dc) == 'actual/360')
	expect_true(dc == 'actual/360')
})

test_that("it should test daycount dib", {
	dc <- as.daycount('actual/360')
	expect_true(dib(dc) == 360)
})

test_that("it should raise an error", {
	expect_error(as.daycount('actual'), 'Unknown daycount: actual')
})

test_that("it should call timefactor with a fixed period", {
	dc <- as.daycount('actual/360')
	expect_true(timefactor(dc, '6 months') == 1/2)
	expect_true(timefactor(dc, 6, units='months') == 1/2)
	expect_true(timefactor(dc, as.term('6 months')) == 1/2)
	expect_true(timefactor(dc, '1 days') == 1/360)
	expect_true(timefactor(dc, 1, units='days') == 1/360)
	expect_true(timefactor(dc, as.term('1 days')) == 1/360)
})

