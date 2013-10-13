
context('SpotRateCurve calculations')

terms <- c(1, 11, 26, 27, 28)
rates <- c(0.0736, 0.0724, 0.0723, 0.0723, 0.0723)

test_that("it should create an interest rate curve", {
    curve <- SpotRateCurve(rates, terms, dib=360, compounding='compounded')
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
    # curve <- SpotRateCurve(numeric(0), numeric(0))
    # expect_equal( length(curve), 0 )
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

test_that('it should find the neighbors for a given term', {
    curve <- SpotRateCurve(rates=seq(1, 0.98, length.out=10),
        terms=c(1, seq(21, by=21, length.out=9)))
    expect_equal(neighbors(curve, 30), c(21, 42))
    expect_equal(neighbors(curve, 21), c(21, 21))
})