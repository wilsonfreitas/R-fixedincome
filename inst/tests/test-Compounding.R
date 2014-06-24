
context('compounding functions')

test_that("it should create a compounding with a string", {
	comp <- as.compounding('simple')
	expect_true(comp == 'simple')
	expect_is(comp, 'compounding')
})

test_that("it should compute discrete compounding", {
	comp <- discreteCompounding()
	expect_true(comp == 'discrete')
	expect_equal(compound(comp, 0.05, 2), 1.1025)
	expect_equal(rates(comp, 1.1025, 2), 0.05)
})

test_that("it should compute simple compounding", {
	comp <- simpleCompounding()
	expect_true(comp == 'simple')
	expect_equal(compound(comp, 0.05, 2), 1.1)
	expect_equal(rates(comp, 1.1, 2), 0.05)
})

test_that("it should compute Continuous compounding", {
	comp <- continuousCompounding()
	expect_true(comp == 'continuous')
	expect_equal(compound(comp, 0.05, 2), 1.105170918)
	expect_equal(rates(comp, 1.105170918, 2), 0.05)
})
