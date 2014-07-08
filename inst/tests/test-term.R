
context('term class')

test_that("it should create a term object", {
	t <- as.term(6, 'months')
	expect_true(t == 6)
	expect_true(units(t) == 'months')
})

test_that("it should raise error", {
	expect_error(as.term(6, 'nada'))
})

test_that("it should coerce to string", {
	t <- as.term(6, 'months')
	expect_true(as.character(t) == '6 months')
})

test_that("it should coerce to numeric", {
	t <- as.term(6, 'months')
	expect_true(as.numeric(t) == 6)
})

test_that("it should create a term object from a string", {
	t <- as.term('6 months')
	expect_true(t == 6)
	expect_true(units(t) == 'months')
})

test_that("it should raise error for invalid term", {
	expect_error(as.term('nada'))
	expect_error(as.term('1 day'))
})

test_that("it should convert term units", {
	t1 <- as.term('6 months')
	t2 <- as.term(t1, units='months')
	expect_true(t2 == 6)
	t2 <- as.term(t1, units='years')
	expect_true(t2 == 1/2)
	t1 <- as.term('1 years')
	t2 <- as.term(t1, units='months')
	expect_true(t2 == 12)
})

test_that("it should raise error while converts", {
	t1 <- as.term('1 years')
	expect_error(as.term(t1, units='days'))
})

test_that("it should create and convert a term", {
	t1 <- as.term('1 years', units='months')
	expect_true(units(t1) == 'months')
	expect_true(t1 == 12)
})

test_that("it should coerce to string (vectorized)", {
	t <- as.term(7:12, 'months')
	expect_true(as.character(t) == 'terms in months\n7 8 9 10 11 12')
})

