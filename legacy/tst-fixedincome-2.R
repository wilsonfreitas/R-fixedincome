
library(testthat)

source('ts.R')

context('interest rates calculations')

test_that("compound calculations", {
    d = list(r=0.09, t=50)
    expect_that(compound(d)$f, equals(1.012041041))
    expect_that(compound(d, 100)$f, equals(1.024227069))
})

test_that("forward rate calculations", {
    d = list(r=0.09, t=50)
    u = list(r=0.1, t=100)
    expect_that(class(fwd.rate(d, u)), equals('list'))
    expect_that(class(fwd.rate(d, u)$r), equals('numeric'))
    expect_that(fwd.rate(d, u)$r, equals(0.110091743))
})

context('flat forward interpolation calculations')

test_that("flat forward interpolation", {
    load('testcopom.rda')
    
    # returning nodes
    expect_that(flat.forward.rates(r.sample, 1)$r, equals(0.0810))
    expect_that(flat.forward.rates(r.sample, 56)$r, equals(0.0871))
    expect_that(flat.forward.rates(r.sample, 87)$r, equals(0.0901))
    expect_that(flat.forward.rates(r.sample, 238)$r, equals(0.0903))
    
    # interpolating
    expect_that(flat.forward.rates(r.sample, 20)$r, equals(0.08689982))
    expect_that(abs(flat.forward.rates(r.sample, 21)$r-0.08691465) < 1e-5, is_true())
})

test_that("daily forward rate calculations", {
    load('testcopom.rda')
    expect_that(forward.rates(r.sample, 1)$r, equals(0.0810))
    expect_that(abs(forward.rates(r.sample, 21)$r-0.08721129) < 1e-5, is_true())
})

context('COPOM interpolation')

test_that("copom flat forward interpolation calculations", {
    load('testcopom.rda')
    
    ffc.rates <- flat.forward.copom.rates(head(r.sample, 2), copom.dates[c(1,2)])
    r2r <- function(.) list(r=.$Rate, t=.$Days)
    comp.3 <- compound(r2r(ffc.rates[3,]))
    comp.fwd.34 <- compound(fwd.rate(r2r(ffc.rates[3,]), r2r(ffc.rates[4,])), ffc.rates[4,'Days']-ffc.rates[3,'Days'])
    fwd.rate.4 <- implied.rate(compound.mul(comp.3, comp.fwd.34), ffc.rates[4,'Days'])$r
    
    expect_that(ffc.rates[1,'Rate']-r.sample[1,'Rate']<1e-5, is_true())
    expect_that(ffc.rates[2,'Rate']-r.sample[1,'Rate']<1e-5, is_true())
    expect_that(ffc.rates[3,'Rate']-r.sample[2,'Rate']<1e-5, is_true())
    expect_that(ffc.rates[4,'Rate']-fwd.rate.4<1e-5, is_true())
})

