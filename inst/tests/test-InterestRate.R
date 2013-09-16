
context('Compounding Functions')

test_that("it should compute Compounded compounding", {
    Compounded <- Compounding[['compounded']]
    expect_true(Compounded == 'compounded')
    expect_that(attr(Compounded, 'compound')(0,1,1), equals(1))
    expect_that(attr(Compounded, 'implied.rate')(1,1,1), equals(0))
})

test_that("it should compute Simple compounding", {
    Simple <- Compounding[['simple']]
    expect_true(Simple == 'simple')
    expect_that(attr(Simple, 'compound')(0,1,1), equals(1))
    expect_that(attr(Simple, 'implied.rate')(1,1,1), equals(0))
})

test_that("it should compute Continuous compounding", {
    Continuous <- Compounding[['continuous']]
    expect_true(Continuous == 'continuous')
    expect_that(attr(Continuous, 'compound')(0,1,1), equals(1))
    expect_that(attr(Continuous, 'implied.rate')(1,1,1), equals(0))
})

context('SpotRate class')

test_that("it should create a SpotRate", {
    spr <- SpotRate(1, 1)
    expect_that(spr$value, equals(1))
    expect_that(spr$term, equals(1))
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
    # expect_that(CompoundFactor(1, 1), not_equals(CompoundFactor(1, 0)))
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

context('SpotRateCurve calculations')

test_that("it should create an interest rate curve", {
    curve <- SpotRateCurve(numeric(0), numeric(0))
    expect_is( curve, "SpotRateCurve" )
})

test_that("it should verify if terms and rates have the same length", {
    expect_error( SpotRateCurve(numeric(10), numeric(11)) )
})

test_that("it should access the rates attribute", {
    curve <- SpotRateCurve(rates=seq(1, 0.98, length.out=10),
                    terms=c(1, seq(21, 100, length.out=9)))
    expect_equal( rates(curve), seq(1, 0.98, length.out=10) )
})

test_that("it should return the interest rate curve length", {
    curve <- SpotRateCurve(numeric(0), numeric(0))
    expect_equal( length(curve), 0 )
    curve <- SpotRateCurve(rates=seq(1, 0.98, length.out=10),
                    terms=c(1, seq(21, 100, length.out=9)))
    expect_equal( length(curve), 10 )
})

test_that("it should access the elements by its indexes", {
    curve <- SpotRateCurve(rates=seq(1, 0.98, length.out=10),
                    terms=c(1, seq(21, by=21, length.out=9)))
    expect_equal( curve[1], SpotRate(1,1) )
    expect_equal( curve[21], SpotRate(0.9977778, 21), tolerance=1e-6 )
    expect_equal( curve[40], SpotRate(0.9956666, 40) )
})

test_that("it should compute forward rates", {
    curve <- SpotRateCurve(rates=seq(1, 0.98, length.out=10),
                    terms=c(1, seq(21, by=21, length.out=9)))
    expect_equal( forward.rate(curve, 1, 1), SpotRate(1,1) )
    expect_equal( curve[1, 1], SpotRate(1,1) )
})

test_that('it should append a SpotRate to a SpotRateCurve', {
    curve <- SpotRateCurve(rates=seq(0.99, 0.98, length.out=4), terms=seq(2,5))
    expect_equal(length(curve), 4)
    insert(curve) <- SpotRate(1,1)
    expect_equal(length(curve), 5)
    expect_equal(terms(curve), 1:5)
    expect_equal(rates(curve), c(1, seq(0.99, 0.98, length.out=4)))
    
    curve <- SpotRateCurve(rates=c(0.9, 0.8, 0.6, 0.5), terms=c(1,2,4,5))
    insert(curve) <- SpotRate(0.7, 3)
    expect_equal(terms(curve), 1:5)
    expect_equal(rates(curve), c(0.9, 0.8, 0.7, 0.6, 0.5))

    expect_error( insert(1) <- 2 )
})

context('Interpolation functions')

test_that("it should interpolate FlatForwardly the interest rate curve ", {
    curve <- SpotRateCurve(rates=seq(1, 0.98, length.out=10),
                    terms=c(1, seq(21, by=21, length.out=9)))
    expect_equal( interp.FlatForward(curve, 1), 1 )
    expect_equal( interp.FlatForward(curve, 21), 0.9977778, tolerance=1e-6)
    expect_equal( interp.FlatForward(curve, 40), 0.9956666 )
    expect_error( interp.FlatForward(0) )
})

