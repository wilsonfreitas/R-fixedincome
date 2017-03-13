
context('compounding functions')

test_that("it should create a compounding class", {
  expect_is(compounding("simple"), "compounding")
  expect_is(compounding("simple"), "simple")
  expect_is(compounding("discrete"), "compounding")
  expect_is(compounding("discrete"), "discrete")
  expect_is(compounding("continuous"), "compounding")
  expect_is(compounding("continuous"), "continuous")
  expect_error(compounding("nada"))
})

test_that("it should compute compounding factor", {
  expect_equal(compound("simple", 2, 0.05), 1.1)
  expect_equal(compound("discrete", 2, 0.05), 1.1025)
  expect_equal(compound("continuous", 2, 0.05), 1.105170918)
})

test_that("it should compute compounding implied rate", {
  expect_equal(rates("simple", 2, 1.1), 0.05)
  expect_equal(rates("discrete", 2, 1.1025), 0.05)
  expect_equal(rates("continuous", 2, 1.105170918), 0.05)
})

test_that("it should coece compounding to character", {
  expect_equal(as(compounding("simple"), "character"), "simple")
  expect_equal(as(compounding("discrete"), "character"), "discrete")
  expect_equal(as(compounding("continuous"), "character"), "continuous")
})

