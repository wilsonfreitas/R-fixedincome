
# context('term class')

test_that("it should create a term object", {
  t <- term(6, 'months')
  expect_s4_class(t, "Term")
  expect_s4_class(t, 'Term')
  expect_true(t@units == 'month')
  t <- term(1:10, "days")
  expect_true(all(t@units == 'day'))
})

test_that("it should test equality", {
  t <- term(6, 'months')
  expect_true(t == "6 months")
  expect_true("6 months" == t)
  t <- term(1, 'months')
  expect_true(t == "1 month")
  expect_true("1 month" == t)
})

test_that("it should coerce term to numeric", {
  t <- term(6, 'months')
  expect_equal(as.numeric(t), 6)
})

test_that("it should coerce term to character", {
  t <- term(6, 'month')
  expect_equal(as(t, "character"), "6 months")
  t <- term(6:9, 'month')
  expect_equal(as(t, "character"),
               c("6 months", "7 months", "8 months", "9 months"))
  t <- term(6:9, c("month", "year"))
  expect_equal(as(t, "character"),
               c("6 months", "7 years", "8 months", "9 years"))
})

test_that("it should raise error", {
  expect_error(term(6, 'nada'))
})

test_that("it should create a term object from a string", {
  t <- as.term('6 month')
  expect_true(as.numeric(t) == 6)
  expect_true(t@units == 'month')
  expect_error(as.term('nada'))
})

test_that("it should check the length of a term", {
  t <- term(1, "day")
  expect_equal(length(t), 1)
  t <- term(1:10, "day")
  expect_equal(length(t), 10)
})

test_that("it should access elements of a term", {
  t <- term(1, "day")
  expect_equal(t[1], term(1, "day"))
  t <- term(1:10, "day")
  expect_equal(t[c(5,6)], term(c(5,6), "day"))
})

test_that("it should put a term object into a data.frame column", {
  t <- term(1:10, "day")
  df <- data.frame(term = t)
  expect_s3_class(df, "data.frame")
  expect_s4_class(df$term, "Term")
  expect_equal(df[1,"term"], term(1, "day"))
  expect_equal(df[c(T, F),"term"], term(c(1, 3, 5, 7, 9), "day"))
})

test_that("it should create terms with different units", {
  t1 <- term(c(1, 2), c("day", "month"))
  expect_equal(t1@units, c("day", "month"))
})

test_that("it should compare terms with different units", {
  t1 <- term(1, "day")
  t2 <- term(1, "year")
  expect_true(t1 < t2)
  expect_false(t1 > t2)
  t1 <- term(c(1, 2), c("day", "year"))
  t2 <- term(1, "month")
  expect_equal(t1 < t2, c(TRUE, FALSE))
})

test_that("it should create a DateRangeTerm", {
  t <- term(as.Date("2022-02-14"), as.Date("2022-02-18"), "actual")
  expect_equal(t@units, "day")
  expect_equal(as(t, "character"), "4 days")
  expect_equal(as(t, "numeric"), 4)
})

test_that("it should shift a term object", {
  t <- term(1:5, "days")
  st <- shift(t)
  expect_s4_class(st, "Term")
  expect_equal(as.numeric(st), c(NA, 1, 2, 3, 4))
  
})

test_that("it should concatenate terms", {
  t1 <- term(1:5, "days")
  t2 <- term(6:10, "days")
  t <- c(t1, t2)
  expect_s4_class(t, "Term")
  expect_equal(as.numeric(t), 1:10)
})

# test_that("it should concatenate numeric with terms", {
#   t2 <- term(6:10, "days")
#   t <- c(1:5, t2)
#   expect_s4_class(t, "Term")
#   expect_equal(as.numeric(t), 1:10)
# })

test_that("it should diff terms", {
  t1 <- term(1:5, "days")
  t <- diff(t1)
  expect_s4_class(t, "Term")
  expect_length(t, 4)
  expect_equal(as.numeric(t), rep(1, 4))
  t1 <- term(1:5, "days")
  t <- diff(t1, fill = NA)
  expect_s4_class(t, "Term")
  expect_length(t, 5)
  expect_equal(as.numeric(t), c(NA, rep(1, 4)))
})

test_that("it should compare terms with different units", {
  t1 <- term(252, "days", "business/252")
  t2 <- term(1, "year", "business/252")
  expect_true(t1 == t2)
  t1 <- term(12, "months", "business/252")
  t2 <- term(1, "year", "business/252")
  expect_true(t1 == t2)
  t1 <- term(12, "months", "actual/360")
  t2 <- term(1, "year", "business/252")
  expect_true(t1 != t2)
})
