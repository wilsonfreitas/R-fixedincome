context('CompoundFactor class')

test_that("it should create a CompoundFactor", {
    expect_that(class(CompoundFactor(1, 1)), equals('CompoundFactor'))
})

test_that("it should access CompoundFactor attributes (thru its methods)", {
    expect_that(compound.factor(CompoundFactor(1, 1)), equals(1))
    expect_that(term(CompoundFactor(1, 1)), equals(1))
})

test_that("it should check for CompoundFactor equality", {
    expect_that(all.equal(CompoundFactor(1, 1), CompoundFactor(1, 1)), equals(TRUE))
    expect_that(all.equal(CompoundFactor(1, 1), CompoundFactor(0, 1)), equals(FALSE))
    expect_that(CompoundFactor(1, 1), equals(CompoundFactor(1, 1)))
})

test_that('it should compute SpotRate for a given CompoundFactor (implied rate)', {
    expect_that(as.SpotRate(CompoundFactor(1, 1)), equals(SpotRate(0, 1)))
    expect_that(as.SpotRate(CompoundFactor(1, 1), dib=360, compounding='simple'),
        equals(SpotRate(0, 1, 360, 'simple')))
    expect_that(as.SpotRate(CompoundFactor(1, 1), dib=365, compounding='continuous'),
        equals(SpotRate(0, 1, 365, 'continuous')))
})

test_that('it should compute the implied rate for a given CompoundFactor', {
    expect_that(rate(CompoundFactor(1, 1)), equals(0))
    expect_that(rate(CompoundFactor(1, 1), dib=360, compounding='simple'),
        equals(0))
    expect_that(rate(CompoundFactor(1, 1), dib=365, compounding='continuous'),
        equals(0))
    expect_that( rate(CompoundFactor(1.001, 252)), equals(0.001) )
    expect_that( rate(CompoundFactor(1.001, 252), compounding='simple'), equals(0.001) )
    expect_that( rate(CompoundFactor(1.001, 252), compounding='continuous'), equals(0.001, tolerance=0.001) )
    expect_that( rate(CompoundFactor(1.001, 1)), equals(0.286434044) )
    expect_that( rate(CompoundFactor(1.001, 1), compounding='simple'), equals(0.252) )
    expect_that( rate(CompoundFactor(1.001, 1), compounding='continuous'), equals(0.251874084) )
})

test_that('it should compose CompoundFactors', {
    expect_that(compound(CompoundFactor(1, 1), CompoundFactor(1, 1)), equals(CompoundFactor(1, 2)))
    expect_that(compound(CompoundFactor(1, 1), CompoundFactor(1, 1)),
        equals(CompoundFactor(1, 1)*CompoundFactor(1, 1)))
    expect_that(CompoundFactor(1, 1)*CompoundFactor(1, 1), equals(CompoundFactor(1, 2)))
})

test_that('it should discount CompoundFactors', {
    expect_that(discount(CompoundFactor(1, 1), CompoundFactor(1, 1)), equals(CompoundFactor(1, 0)))
    expect_that(discount(CompoundFactor(1, 1), CompoundFactor(1, 2)),
        equals(CompoundFactor(1, 2)/CompoundFactor(1, 1)))
    expect_that(CompoundFactor(1, 1)/CompoundFactor(1, 1), equals(CompoundFactor(1, 0)))
})

