
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
