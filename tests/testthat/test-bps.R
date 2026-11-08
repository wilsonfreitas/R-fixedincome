sr_ <- function(x) spotrate(x, "discrete", "business/252", "Brazil/ANBIMA")

test_that("it should create Bps objects (#33)", {
  b <- bps(c(10, 25))
  expect_s4_class(b, "Bps")
  expect_equal(as.numeric(b), c(0.001, 0.0025))
  expect_equal(format(b), c("10 bps", "25 bps"))
  expect_equal(b[2], bps(25))
  expect_equal(as.bps(0.001), bps(10))
})

test_that("it should shift SpotRate objects with Bps (#33)", {
  sr <- sr_(0.1375)
  expect_equal(sr + bps(10), sr_(0.1385))
  expect_equal(bps(10) + sr, sr_(0.1385))
  expect_equal(sr - bps(c(10, 25)), sr_(c(0.1365, 0.1350)))
  expect_error(sr * bps(10), "Only \\+ and -")
  expect_error(bps(10) - sr, "Only \\+")
  expect_equal(as.numeric(as.bps(sr_(0.1375) - sr_(0.1325))) * 1e4, 50)
})

test_that("it should operate Bps with Bps (#33)", {
  expect_equal(bps(10) + bps(5), bps(15))
  expect_equal(bps(10) - bps(5), bps(5))
  expect_equal(bps(10) / bps(5), 2)
  expect_error(bps(10) * bps(5), "not defined")
})

test_that("it should shift a curve and its interpolation (#33)", {
  crv <- spotratecurve(
    c(0.10, 0.11, 0.12), c(1, 21, 42),
    "discrete", "business/252", "Brazil/ANBIMA",
    refdate = as.Date("2022-08-05")
  )
  interpolation(crv) <- interp_flatforward()
  shifted <- crv + bps(100)
  expect_s4_class(shifted, "SpotRateCurve")
  expect_equal(shifted@terms, crv@terms)
  expect_equal(as.numeric(shifted[[21]]), 0.12)
  # interpolated rates come from the shifted curve
  expect_true(as.numeric(shifted[[30]]) > as.numeric(crv[[30]]) + 0.009)
  # the same holds for plain numbers
  expect_equal(as.numeric((crv + 0.01)[[21]]), 0.12)
})

test_that("c() combines Bps objects", {
  expect_equal(c(bps(10), bps(c(20, 30))), bps(c(10, 20, 30)))
  expect_error(c(bps(10), 5), "use bps\\(\\) first")
})
