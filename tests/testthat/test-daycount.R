
# context('daycount class')

test_that("it should create a daycount object", {
  dc <- daycount("actual/360")
  expect_s4_class(dc, "Daycount")
  expect_true(as.character(dc) == "actual/360")
})

test_that("it should test daycount dib", {
  dc <- daycount("actual/360")
  expect_true(dib(dc) == 360)
})

# test_that("it should raise an error", {
#   expect_error(as.daycount('actual'), 'Unknown daycount: actual')
# })

test_that("it should call toyears with a fixed period", {
  dc <- daycount("actual/360")
  # day ----
  t1 <- term(1, "day")
  t <- todays(dc, t1)
  expect_s4_class(t, "Term")
  expect_equal(t@units, "day")
  expect_equal(as.numeric(t), 1)
  t <- tomonths(dc, t1)
  expect_s4_class(t, "Term")
  expect_equal(t@units, "month")
  expect_equal(as.numeric(t), 1 / 30)
  t <- toyears(dc, t1)
  expect_s4_class(t, "Term")
  expect_equal(t@units, "year")
  expect_equal(as.numeric(t), 1 / 360)
  # month ----
  t1 <- term(1, "month")
  t <- todays(dc, t1)
  expect_s4_class(t, "Term")
  expect_equal(t@units, "day")
  expect_equal(as.numeric(t), 30)
  t <- tomonths(dc, t1)
  expect_s4_class(t, "Term")
  expect_equal(t@units, "month")
  expect_equal(as.numeric(t), 1)
  t <- toyears(dc, t1)
  expect_s4_class(t, "Term")
  expect_equal(t@units, "year")
  expect_equal(as.numeric(t), 1 / 12)
  # year ----
  t1 <- term(1, "year")
  t <- todays(dc, t1)
  expect_s4_class(t, "Term")
  expect_equal(t@units, "day")
  expect_equal(as.numeric(t), 360)
  t <- tomonths(dc, t1)
  expect_s4_class(t, "Term")
  expect_equal(t@units, "month")
  expect_equal(as.numeric(t), 12)
  t <- toyears(dc, t1)
  expect_s4_class(t, "Term")
  expect_equal(t@units, "year")
  expect_equal(as.numeric(t), 1)
})

test_that("it should coerce a daycount to character", {
  expect_equal(as(daycount("actual/365"), "character"), "actual/365")
})

test_that("it should raise an error for invalid specification", {
  expect_error(daycount("actual"))
})

# test_that("it should convert days to years term", {
#   dc <- as.daycount('actual/360')
#   expect_equal(as.term(dc, '10 days', 'years'), as.term(10/360, 'years'))
#   expect_equal(as.term(dc, '10 days', 'months'), as.term(10/30, 'months'))
#   expect_equal(as.term(dc, '2 years', 'days'), as.term(360*2, 'days'))
#   expect_equal(as.term(dc, '10 months', 'days'), as.term(300, 'days'))
#   expect_equal(as.term(dc, '2 years', 'months'), as.term(24, 'months'))
#   expect_equal(as.term(dc, '10 months', 'years'), as.term(10/12, 'years'))
# })
#
# test_that('it should test fixed period day counts', {
#   dc <- as.daycount('fixed/22')
#   expect_equal(dib(dc), 22)
# })
test_that("it should reject daycounts it can't compute (#5)", {
  expect_error(daycount("30/360"), "Unsupported daycount")
  expect_s4_class(daycount("business/252"), "Daycount")
  expect_s4_class(daycount("actual/365"), "Daycount")
})

test_that("it should count days with 30E/360 (#5)", {
  dc <- daycount("30E/360")
  expect_equal(dib(dc), 360)
  d <- function(x) as.Date(x)
  # the 31st is moved to the 30th
  expect_equal(count_days(dc, d("2024-01-31"), d("2024-03-31")), 60)
  expect_equal(count_days(dc, d("2024-01-30"), d("2024-01-31")), 0)
  # February has 30 days too
  expect_equal(count_days(dc, d("2024-02-28"), d("2024-03-31")), 32)
  expect_equal(count_days(dc, d("2023-12-15"), d("2024-06-15")), 180)
  expect_equal(count_days(dc, d("2024-01-01"), d("2025-01-01")), 360)
  # vectors
  expect_equal(
    count_days(dc, d("2024-01-15"), d(c("2024-02-15", "2024-07-31"))),
    c(30, 195)
  )
  # other daycounts still use the calendar
  expect_equal(
    count_days(daycount("actual/360"), d("2024-01-31"), d("2024-03-31"), "actual"),
    60
  )
  expect_equal(
    count_days(daycount("actual/360"), d("2024-01-01"), d("2024-03-01"), "actual"),
    60
  )
})

test_that("it should compound with 30E/360 dates (#5)", {
  sr <- spotrate(0.05, "simple", "30E/360", "actual")
  expect_equal(
    compound(sr, as.Date("2024-01-31"), as.Date("2024-03-31")),
    1 + 0.05 * 60 / 360
  )
  expect_equal(
    discount(sr, as.Date("2024-01-01"), as.Date("2025-01-01")),
    1 / 1.05
  )
  expect_equal(compound(sr, term(1, "year")), 1.05)
  expect_equal(as.numeric(todays(daycount("30E/360"), term(6, "months"))), 180)
})
