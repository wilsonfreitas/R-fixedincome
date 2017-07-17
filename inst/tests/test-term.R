
context('term class')

test_that("it should create a term object", {
  t <- term(6, 'months')
  expect_is(t, "term")
  expect_s4_class(t, 'term')
  expect_true(units(t) == 'month')
  t <- term(1:10, "days")
  expect_true(all(units(t) == 'day'))
})

test_that("it should test equality", {
  t <- term(6, 'months')
  expect_true(t == "6 months")
  expect_true("6 months" == t)
})

test_that("it should coerce term to numeric", {
  t <- term(6, 'months')
  expect_equal(as.numeric(t), 6)
})

test_that("it should coerce term to character", {
  t <- term(6, 'month')
  expect_equal(as(t, "character"), "6 month")
  t <- term(6:9, 'month')
  expect_equal(as(t, "character"), c("6 month", "7 month", "8 month", "9 month"))
  t <- term(6:9, c("month", "year"))
  expect_equal(as(t, "character"), c("6 month", "7 year", "8 month", "9 year"))
})

test_that("it should raise error", {
  expect_error(term(6, 'nada'))
})

test_that("it should create a term object from a string", {
  t <- as.term('6 month')
  expect_true(as.numeric(t) == 6)
  expect_true(units(t) == 'month')
  expect_error(as.term('nada'))
})

test_that("it should put a term object into a data.frame column", {
  t <- term(1:10, "day")
  df <- data.frame(term = t)
  expect_is(df, "data.frame")
  expect_is(df$term, "term")
  expect_equal(df[1,"term"], term(1, "day"))
  expect_equal(df[c(T, F),"term"], term(c(1, 3, 5, 7, 9), "day"))
})
