context('formulas')

test_that("it should compute a future value", {
	spr <- as.spotrate(0.1, as.compounding('discrete'), as.daycount('actual/365'))
	expect_equal(FV(spr, 1, 100, 0, FALSE), 100)
	expect_equal(FV(spr, 1, 100, 0, TRUE), 110)
	expect_equal(FV(spr, 1, 100, 100, FALSE), 210)
	expect_equal(FV(spr, 1, 100, 100, TRUE), 220)
})

test_that("it should compute the present value", {
	spr <- as.spotrate(0.1, as.compounding('discrete'), as.daycount('actual/365'))
	expect_equal(PV(spr, 1, 100, 0, FALSE), -90.90909091)
	expect_equal(PV(spr, 1, 100, 0, TRUE), -100)
	expect_equal(PV(spr, 1, 100, 100, TRUE), -190.90909091)
	expect_equal(PV(spr, 1, 100, 100, FALSE), -181.81818182)
})

test_that("it should compute the rate", {
	expect_equal(RATE(1, 100, -90, 0, FALSE), 0.1111119211)
})

