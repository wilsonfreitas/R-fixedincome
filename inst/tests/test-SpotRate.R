
context('spotrate class')

test_that("it should create a spotrate", {
  spr <- spotrate(0.06, 'simple', 'actual/365')
  expect_is(spr, 'spotrate')
  expect_equal(spr@value, 0.06)
  expect_equal(spr@daycount@spec, "actual/365")
  expect_is(spr@daycount, "daycount")
  expect_is(spr@compounding, "compounding")
  expect_equal(spr@calendar, "actual")
})

test_that("it should compute a compounding factor for fixed periods", {
  spr <- spotrate(0.06, 'simple', 'actual/365')
  expect_equal(compound(spr, 10, "days"), 1.001643836)
  expect_equal(compound(spr, 1, "years"), 1.06)
  expect_equal(compound(spr, term(10, "days")), 1.001643836)
  expect_equal(compound(spr, rep(10, 5), "days"), rep(1.001643836, 5))
  expect_equal(compound(spr, term(rep(10, 5), "days")), rep(1.001643836, 5))

  expect_equal(compound(spr, Sys.Date(), Sys.Date()+10), 1.001643836)
  expect_equal(compound(spr, Sys.Date()+(0:9), Sys.Date()+(10:19)), rep(1.001643836, 10))
})

test_that("it should check for equality", {
  spr <- spotrate(0.06, 'simple', 'actual/365')
  spr2 <- spotrate(0.06, 'simple', 'actual/365')
  # spotrate vs spotrate
  expect_true(spr == spr2)
  expect_true(spr >= spr2)
  expect_true(spr <= spr2)
  expect_false(spr != spr2)
  expect_false(spr < spr2)
  expect_false(spr > spr2)
  # spotrate vs numeric
  expect_true(spr ==  0.06)
  expect_true(spr >=  0.06)
  expect_true(spr <=  0.06)
  expect_false(spr != 0.06)
  expect_false(spr <  0.06)
  expect_false(spr >  0.06)
  # numeric vs spotrate
  expect_true(0.06 == spr2)
  expect_true(0.06 >= spr2)
  expect_true(0.06 <= spr2)
  expect_false(0.06 != spr2)
  expect_false(0.06 < spr2)
  expect_false(0.06 > spr2)
  
  spr <- spotrate(c(0.06, 0.07, 0.08), 'simple', 'actual/365')
  expect_true(all(spr == c(0.06, 0.07, 0.08)))
})

test_that("it should do arithmetic operations with spotrate", {
  spr <- spotrate(0.06, 'simple', 'actual/365')
  spr2 <- spotrate(0.06, 'simple', 'actual/365')
  # spotrate vs spotrate
  expect_is(spr + spr2, "spotrate")
  expect_true((spr + spr2)@value == 0.12)
  # check type
  expect_is(spr + 0.01, "spotrate")
  expect_is(spr - 0.01, "spotrate")
  expect_is(spr * 0.01, "spotrate")
  expect_is(spr / 0.01, "spotrate")
  expect_is(spr ^ 0.01, "spotrate")
  # spotrate vs numeric
  expect_true((spr + 0.06)@value == 0.12)
  # numeric vs spotrate
  expect_true((0.06 + spr)@value == 0.12)
  # vectorized
  spr <- spotrate(c(0.06, 0.07, 0.08), 'simple', 'actual/365')
  expect_true( all((spr + 0.01) - (c(0.06, 0.07, 0.08)+0.01) < 1e-10) )
})

test_that("it should create a spotrate with NAs", {
  spr <- spotrate(NA, 'simple', 'actual/365')
  expect_true(is.na(spr))
  spr <- spotrate(Inf, 'simple', 'actual/365')
  expect_true(is.infinite(spr))
  spr <- spotrate(c(0.06, NA, 0.08), 'simple', 'actual/365')
  expect_equal(is.na(spr), c(F, T, F))
})

test_that("it should discount a spot rate", {
  spr <- spotrate(0.06, 'simple', 'actual/365')
  expect_equal(discount(spr, 10, "days"), 0.9983588618)
  spr <- spotrate(0.06, 'continuous', 'actual/365')
  expect_equal(discount(spr, as.Date('2013-01-01'), as.Date('2013-01-02')), 0.99983563)
})

test_that("it should test zero term", {
  spr <- spotrate(0.06, 'simple', 'actual/365')
  expect_equal(compound(spr, 0, 'days'), 1)
  expect_equal(compound(spr, 0, 'years'), 1)
})

test_that("it should test index operator", {
  spr <- spotrate(0.06, 'simple', 'actual/365')
  expect_equal(spr, spr[1])
  spr <- spotrate(c(0.06, 0.07), 'simple', 'actual/365')
  expect_true(spr[1] == 0.06)
  spr[1] <- 0.08
  expect_true(all(spr == c(0.08, 0.07)))
})

test_that("it should take the length of a spotrate", {
  spr <- spotrate(0.06, 'simple', 'actual/365')
  expect_equal(length(spr), 1)
  spr <- spotrate(c(0.06, 0.07), 'simple', 'actual/365')
  expect_equal(length(spr), 2)
})

test_that("it should combine spotrate values into a vector", {
  spr <- spotrate(0.06, 'simple', 'actual/365')
  expect_is(append(spr, 0.07), "spotrate")
  expect_equal(length(append(spr, 0.07)), 2)
  spr2 <- spotrate(0.07, 'simple', 'actual/365')
  expect_is(append(spr, spr2), "spotrate")
  expect_equal(length(append(spr, spr2)), 2)
  expect_true(all( append(spr, spr2, after = 0L) == c(0.07, 0.06)))
})

test_that("it should coerce the spotrate object into others modes", {
  spr <- spotrate(0.06, 'simple', 'actual/365')
  expect_equal(as.numeric(spr), 0.06)
  expect_equal(as(spr, "numeric"), 0.06)
  expect_equal(as.character(spr), "0.06 simple actual/365 actual")
  expect_equal(as(spr, "character"), "0.06 simple actual/365 actual")
  # spotrate vector
  spr <- spotrate(c(0.06, 0.07, 0.08, 0.09), 'continuous', 'actual/365', 'actual')
  vx <- paste(c(0.06, 0.07, 0.08, 0.09), 'continuous actual/365 actual')
  expect_equal(as(spr, "character"), vx)
})

test_that("it should parse a string to build a spotrate", {
  spr <- as.spotrate('0.06 simple actual/365 actual')
  expect_is(spr, 'spotrate')
  expect_true(spr@value == 0.06)
  expect_true(is(spr@compounding, "simple"))
  expect_true(spr@daycount@spec == 'actual/365')
  expect_true(spr@calendar == "actual")
  # vectorized
  spr <- as.spotrate(c('0.06 simple actual/365 actual', '0.07 simple actual/365 actual'))
  expect_is(spr, 'spotrate')
  expect_true(length(spr) == 2)
  expect_equal(spr@value, c(0.06, 0.07))
  expect_true(is(spr@compounding, "simple"))
  expect_true(spr@daycount@spec == 'actual/365')
  expect_true(spr@calendar == "actual")
  # vectorized 2
  spr <- as.spotrate(c('0.06 discrete actual/365 actual', '0.07 simple actual/365 actual'))
  expect_is(spr, 'list')
  expect_true(length(spr) == 2)
  expect_is(spr[[2]]@compounding, "simple")
  expect_true(spr[[1]]@daycount@spec == 'actual/365')
  expect_true(spr[[1]]@calendar == "actual")
  # simplify = FALSE
  spr <- as.spotrate('0.06 simple actual/365 actual', simplify = FALSE)
  expect_is(spr, 'list')
  expect_true(length(spr) == 1)
  expect_equal(sapply(spr, function(x) x@value), 0.06)
  spr <- as.spotrate(c('0.06 simple actual/365 actual', '0.07 simple actual/365 actual'), simplify = FALSE)
  expect_is(spr, 'list')
  expect_true(length(spr) == 2)
  expect_equal(sapply(spr, function(x) x@value), c(0.06, 0.07))
})

test_that("it should create the spotrate object copying some attributes of other object", {
  spr <- as.spotrate('0.06 simple actual/365 actual')
  spr2 <- spotrate(0.07, copyfrom = spr)
  expect_equal(spr@compounding, spr2@compounding)
  expect_equal(spr@daycount, spr2@daycount)
  expect_equal(spr@calendar, spr2@calendar)
  expect_equal(spr2@value, 0.07)
  # 
  spr2 <- spotrate(c(0.07, 0.08), "continuous", copyfrom = spr)
  expect_is(spr2@compounding, "continuous")
  expect_equal(length(spr2), 2)
  expect_equal(spr@daycount, spr2@daycount)
  expect_equal(spr@calendar, spr2@calendar)
  expect_equal(spr2@value, c(0.07, 0.08))
})

test_that("it should convert a spotrate to another spotrate", {
  spr_s <- spotrate(0.06, 'simple', 'actual/365')
  spr_d <- convert(spr_s, term(1, 'years'), .compounding = 'discrete')
  expect_equal(as.numeric(spr_d), 0.06)
  # simple change
  spr_d <- convert(spr_s, term(1, 'months'), .compounding = 'discrete')
  expect_equal(as.numeric(spr_d), 0.06167781186)
  # vectorized
  spr_s <- spotrate(c(0.06, 0.07, 0.08), 'simple', 'actual/365')
  spr_d <- convert(spr_s, term(1, 'months'), .compounding = 'discrete')
  expect_equal(as.numeric(spr_d), c(0.06167781186, 0.07229008086, 0.08299950681))
  # change daycount
  spr_s <- spotrate(10.68/100, "discrete", "actual/360", "actual")
  spr_d <- convert(spr_s, term(1, 'days'), .daycount = 'business/252', .calendar = "Brazil/ANBIMA")
  expect_equal(as.numeric(spr_d), 0.07361459)
  # change calendar
  spr_s <- spotrate(10.68/100, "simple", "actual/360", "actual")
  spr_d <- convert(spr_s, as.Date("2017-03-03"), as.Date("2017-03-06"), .compounding = "discrete", .daycount = 'business/252', .calendar = "Brazil/ANBIMA")
  expect_equal(as.numeric(spr_d), 0.2512966, tolerance = 1e-6)
  
})

test_that("it should replicate a spotrate", {
  spr <- spotrate(0.06, 'simple', 'actual/365', 'actual')
  x <- rep(spr, 10)
  expect_equal(length(x), 10)
  expect_equal(x@value, rep(0.06, 10))
})

test_that('it should return the spotrate\'s head', {
  # spotrate must obey the length, [, protocol
  spr <- spotrate(0.06, 'simple', 'actual/365', 'actual')
  spr <- append(rep(spr, 10), 0.01, after = 0)
  hr <- head(spr, 3)
  expect_is(hr, 'spotrate')
  expect_equal(length(hr), 3)
  expect_equal(hr@value, c(0.01, 0.06, 0.06))
})

test_that('it should return the spotrate\'s tail', {
  # spotrate must obey the length, [, protocol
  spr <- spotrate(0.06, 'simple', 'actual/365', 'actual')
  spr <- append(rep(spr, 9), 0.01)
  hr <- tail(spr, 3)
  expect_is(hr, 'spotrate')
  expect_equal(length(hr), 3)
  expect_equal(hr@value, c(0.06, 0.06, 0.01))
})
