
library(rb3)
library(bizdays)
library(tidyverse)
library(fixedincome)
library(nloptr)

# refdate <- getdate("last bizday", Sys.Date(), "Brazil/ANBIMA")
refdate <- as.Date("2022-02-23")
yc <- yc_get(refdate)
fut <- futures_get(refdate)
yc_ss <- yc_superset(yc, fut)
yc_ss_first <- yc_ss |> filter(biz_days == 1)
yc_ss_curve <- yc_ss |> filter(!is.na(symbol))
yc_ <- bind_rows(yc_ss_first, yc_ss_curve)

sp_curve <- spotratecurve(
  yc_$r_252, yc_$biz_days,
  "discrete", "business/252", "Brazil/ANBIMA",
  refdate = refdate
)

ggspotratecurveplot(sp_curve)

# ----

build_fobj <- function(par) {
  function(x, val, term) {
    rates_ <- fixedincome:::nss(term, par[1], par[2], x[1], x[2], x[3], x[4])
    sum((val - rates_)^2)
  }
}

# optim ----
beta0 <- as.numeric(fixedincome::last(sp_curve, "1 day"))
beta1 <- as.numeric(sp_curve[1]) - beta0
par <- c(0.01, 0.01, 3, 1)
f_obj <- build_fobj(c(beta0, beta1))
res2 <- optim(par,
  fn = f_obj,
  lower = c(-1, -1,  1e-6, 1e-6),
  upper = c(1,   1,     5,    5),
  method = "L-BFGS-B",
  val = as.numeric(sp_curve),
  term = as.numeric(toyears(sp_curve@daycount, sp_curve@terms)),
  control = list(trace = 3, factr = 1e-8)
)
do.call(interp_nelsonsiegelsvensson, as.list(c(c(beta0, beta1), res2$par))) |> print()
interpolation(sp_curve) <- do.call(interp_nelsonsiegelsvensson, as.list(c(c(beta0, beta1), res2$par)))
sp_curve |> plot(use_interpolation = TRUE)
