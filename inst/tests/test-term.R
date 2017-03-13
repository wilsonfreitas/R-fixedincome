
context('term class')

test_that("it should create a term object", {
  t <- term(6, 'months')
  expect_is(t, "term")
  expect_s4_class(t, 'term')
  expect_true(units(t) == 'months')
  t <- term(1:10, "days")
  expect_true(units(t) == 'days')
})

test_that("it should test equality", {
  t <- term(6, 'months')
  expect_true(t == 6)
  expect_true(6 == t)
})

test_that("it should coerce term to numeric", {
  t <- term(6, 'months')
  expect_equal(as(t, "numeric"), 6)
})

test_that("it should coerce term to character", {
  t <- term(6, 'months')
  expect_equal(as(t, "character"), "6 months")
  t <- term(6:9, 'months')
  expect_equal(as(t, "character"), c("6 months", "7 months", "8 months", "9 months"))
})

test_that("it should raise error", {
  expect_error(term(6, 'nada'))
})

test_that("it should create a term object from a string", {
  t <- as('6 months', "term")
  expect_true(t == 6)
  expect_true(units(t) == 'months')
  t <- as('1.25 years', "term")
  expect_true(t == 1.25)
  expect_true(units(t) == 'years')
  expect_error(as('nada', "term"))
  expect_error(as('1 day', "term"))
})
