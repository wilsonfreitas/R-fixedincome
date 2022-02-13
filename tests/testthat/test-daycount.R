
# context('daycount class')

test_that("it should create a daycount object", {
  dc <- daycount('actual/360')
  expect_s4_class(dc, "Daycount")
  expect_true(as.character(dc) == 'actual/360')
})

test_that("it should test daycount dib", {
  dc <- daycount('actual/360')
  expect_true(dib(dc) == 360)
})

# test_that("it should raise an error", {
#   expect_error(as.daycount('actual'), 'Unknown daycount: actual')
# })

test_that("it should call timefactor with a fixed period", {
  dc <- daycount('actual/360')
  expect_true(timefactor(dc, 1, 'day') == 1/360)
  expect_true(timefactor(dc, '1 day') == 1/360)
  expect_true(timefactor(dc, term(1, "day")) == 1/360)
  expect_true(timefactor(dc, 6, 'month') == 1/2)
  expect_true(timefactor(dc, '6 month') == 1/2)
  expect_true(timefactor(dc, term(6, "month")) == 1/2)
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
