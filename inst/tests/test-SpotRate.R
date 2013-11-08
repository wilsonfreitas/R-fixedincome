
context('SpotRate class')

test_that("it should create a SpotRate", {
    spr <- SpotRate(1, 1)
    expect_that(rate(spr), equals(1))
    expect_that(term(spr), equals(1))
    expect_that(class(spr), equals('SpotRate'))
})

test_that("it should access SpotRate attributes (thru its methods)", {
    expect_that(rate(SpotRate(1, 1)), equals(1))
    expect_that(term(SpotRate(1, 2)), equals(2))
    expect_that(dib(SpotRate(1, 2)), equals(252))
    expect_that(dib(SpotRate(1, 2, 360)), equals(360))
    expect_that(compounding(SpotRate(1, 2, 360)), equals('compounded'))
})

test_that("it should check for SpotRate equality", {
    expect_that(all.equal(SpotRate(1, 1), SpotRate(1, 1)), equals(TRUE))
    expect_that(all.equal(SpotRate(1, 1), SpotRate(1, 0)), equals(FALSE))
    expect_that(SpotRate(1, 1), equals(SpotRate(1, 1)))
    # expect_that(SpotRate(1, 1), not_equals(SpotRate(1, 0)))
})

test_that("it should compute the CompoundFactor based on a SpotRate", {
    expect_that(as.CompoundFactor(SpotRate(0, 1)), equals(CompoundFactor(1, 1)))
})

test_that('it should compute forward rate between two SpotRates', {
    expect_that(forward.rate(SpotRate(0,1), SpotRate(0,1)), equals(SpotRate(0,0)))
    expect_that(forward.rate(SpotRate(0,1), SpotRate(0,2)), equals(SpotRate(0,1)))
    expect_error(forward.rate(SpotRate(0,2), SpotRate(0,1)))
})

