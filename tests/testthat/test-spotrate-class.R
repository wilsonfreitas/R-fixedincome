
# context('spotrate class')

test_that("it should create a spotrate", {
  spr <- spotrate(0.06, 'simple', 'actual/365')
  expect_s4_class(spr, 'SpotRate')
  expect_equal(as.numeric(spr), 0.06)
  expect_equal(as.character(spr@daycount), "actual/365")
  expect_s4_class(spr@daycount, "Daycount")
  expect_s4_class(spr@compounding, "Compounding")
  expect_equal(spr@calendar, "actual")
})

test_that("it should compute a compounding factor for fixed periods", {
  spr <- spotrate(0.06, 'simple', 'actual/365')
  expect_equal(compound(spr, 10, "day"), 1.001643836)
  expect_equal(compound(spr, 1, "year"), 1.06)
  expect_equal(compound(spr, term(10, "day")), 1.001643836)
  expect_equal(compound(spr, rep(10, 5), "day"), rep(1.001643836, 5))
  expect_equal(compound(spr, term(rep(10, 5), "day")), rep(1.001643836, 5))

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
  expect_s4_class(spr + spr2, "SpotRate")
  expect_true(as.numeric(spr + spr2) == 0.12)
  # check type
  expect_s4_class(spr + 0.01, "SpotRate")
  expect_s4_class(spr - 0.01, "SpotRate")
  expect_s4_class(spr * 0.01, "SpotRate")
  expect_s4_class(spr / 0.01, "SpotRate")
  expect_s4_class(spr ^ 0.01, "SpotRate")
  # spotrate vs numeric
  expect_true(as.numeric(spr + 0.06) == 0.12)
  # numeric vs spotrate
  expect_true(as.numeric(0.06 + spr) == 0.12)
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
  expect_s4_class(c(spr, 0.07), "SpotRate")
  expect_equal(length(c(spr, 0.07)), 2)
  spr2 <- spotrate(0.07, 'simple', 'actual/365')
  expect_s4_class(c(spr, spr2), "SpotRate")
  expect_equal(length(c(spr, spr2)), 2)
  expect_true(all( c(spr, spr2) == c(0.07, 0.06)))
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
  expect_s4_class(spr, 'SpotRate')
  expect_true(as.numeric(spr) == 0.06)
  expect_true(is(spr@compounding, "Simple"))
  expect_true(as.character(spr@daycount) == 'actual/365')
  expect_true(spr@calendar == "actual")
  # vectorized
  spr <- as.spotrate(c('0.06 simple actual/365 actual', '0.07 simple actual/365 actual'))
  expect_s4_class(spr, 'SpotRate')
  expect_true(length(spr) == 2)
  expect_equal(as.numeric(spr), c(0.06, 0.07))
  expect_true(is(spr@compounding, "Simple"))
  expect_true(as.character(spr@daycount) == 'actual/365')
  expect_true(spr@calendar == "actual")
  # vectorized 2
  spr <- as.spotrate(c('0.06 discrete actual/365 actual', '0.07 simple actual/365 actual'))
  expect_type(spr, 'list')
  expect_true(length(spr) == 2)
  expect_s4_class(spr[[2]]@compounding, "Simple")
  expect_true(as.character(spr[[1]]@daycount) == 'actual/365')
  expect_true(spr[[1]]@calendar == "actual")
  # simplify = FALSE
  spr <- as.spotrate('0.06 simple actual/365 actual', simplify = FALSE)
  expect_type(spr, 'list')
  expect_true(length(spr) == 1)
  expect_equal(sapply(spr, function(x) as.numeric(x)), 0.06)
  spr <- as.spotrate(c('0.06 simple actual/365 actual', '0.07 simple actual/365 actual'), simplify = FALSE)
  expect_type(spr, 'list')
  expect_true(length(spr) == 2)
  expect_equal(sapply(spr, function(x) as.numeric(x)), c(0.06, 0.07))
})

test_that("it should create the spotrate object copying some attributes of other object", {
  spr <- as.spotrate('0.06 simple actual/365 actual')
  spr2 <- spotrate(0.07, .copyfrom = spr)
  expect_equal(spr@compounding, spr2@compounding)
  expect_equal(spr@daycount, spr2@daycount)
  expect_equal(spr@calendar, spr2@calendar)
  expect_equal(as.numeric(spr2), 0.07)
  # 
  spr2 <- spotrate(c(0.07, 0.08), "continuous", .copyfrom = spr)
  expect_s4_class(spr2@compounding, "Continuous")
  expect_equal(length(spr2), 2)
  expect_equal(spr@daycount, spr2@daycount)
  expect_equal(spr@calendar, spr2@calendar)
  expect_equal(as.numeric(spr2), c(0.07, 0.08))
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
  expect_equal(as.numeric(spr_d), 0.07361459, tolerance = 1e-6)
  # change calendar
  spr_s <- spotrate(10.68/100, "simple", "actual/360", "actual")
  spr_d <- convert(spr_s, as.Date("2017-03-03"), as.Date("2017-03-06"),
                   .compounding = "discrete", .daycount = 'business/252',
                   .calendar = "Brazil/ANBIMA")
  expect_equal(as.numeric(spr_d), 0.2512966, tolerance = 1e-6)
  
})

test_that("it should replicate a spotrate", {
  spr <- spotrate(0.06, 'simple', 'actual/365', 'actual')
  x <- rep(spr, 10)
  expect_equal(length(x), 10)
  expect_equal(as.numeric(x), rep(0.06, 10))
})

test_that('it should return the spotrate\'s head', {
  # spotrate must obey the length, [, protocol
  spr <- spotrate(c(0.01, 0.06), 'simple', 'actual/365', 'actual')
  spr <- c(spr, rep(0.06, 8))
  hr <- head(spr, 3)
  expect_s4_class(hr, 'SpotRate')
  expect_equal(length(hr), 3)
  expect_equal(as.numeric(hr), c(0.01, 0.06, 0.06))
})

test_that('it should return the spotrate\'s tail', {
  # spotrate must obey the length, [, protocol
  spr <- spotrate(0.06, 'simple', 'actual/365', 'actual')
  spr <- tail(rep(spr, 9), 0.01)
  hr <- tail(spr, 3)
  expect_s4_class(hr, 'SpotRate')
  expect_equal(length(hr), 3)
  expect_equal(as.numeric(hr), c(0.06, 0.06, 0.01))
})
