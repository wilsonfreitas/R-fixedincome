
context('creating irc')

#' I have the following alternatives
#' 1. terms in days + refdate so that I could handle dates and days 
#' (according to calendar) interpolation
#' 2. terms in dates + refdate so that I could handle dates and days
#' interpolation
#' 
#' Missing:
#' 1. interpolate by year
#' 

test_that("it should create an interest rate curve", {
	curve <- irc(1:10, rnorm(10))
	expect_is( curve, "irc" )
	curve <- irc(Sys.Date() + 1:10, rnorm(10))
	expect_is( curve, "irc" )
})

test_that("it should create an empty interest rate curve", {
	curve <- irc(1:10, NA)
	expect_is( curve, "irc" )
})

test_that("it should index an interest rate and return a rate", {
	terms <- c(1, 11, 26, 27, 28)
	rates <- c(0.05, 0.055, 0.06, 0.065, 0.07)
	curve <- irc(terms, rates)
	expect_equal(curve[11], 0.055)
})
