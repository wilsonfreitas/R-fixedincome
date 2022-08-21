
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

# ----

f_obj <- function(x, val, term) {
  rates_ <- fixedincome:::nss(term, x[1], x[2], x[3], x[4], x[5], x[6])
  sum((val - rates_)^2)
}

gr_f_obj <- function(x, val, term) {
  d_beta1 <- function(term, l) {
    rep(1, length(term))
  }

  d_beta2 <- function(term, l) {
    (1 - exp(-l * term)) / (l * term)
  }

  d_beta3 <- function(term, l) {
    (1 - exp(-l * term) * (1 + l * term)) / (l * term)
  }

  d_lambda1 <- function(term, l, b2, b3) {
    -(b2 / l) * (1 - exp(-l * term) * (1 + l * term)) / (l * term) -
      (b3 / l) * (1 - exp(-l * term) * (1 + l * term + (l * term)^2)) / (l * term)
  }

  d_lambda2 <- function(term, l, b4) {
    -(b4 / l) * (1 - exp(-l * term) * (1 + l * term + (l * term)^2)) / (l * term)
  }

  rates_ <- fixedincome:::nss(term, x[1], x[2], x[3], x[4], x[5], x[6])
  obj <- f_obj(x, val, term)
  v <- c(
    2 * sum((val - rates_) * -d_beta1(term)),
    2 * sum((val - rates_) * -d_beta2(term, x[5])),
    2 * sum((val - rates_) * -d_beta3(term, x[5])),
    2 * sum((val - rates_) * -d_beta3(term, x[6])),
    2 * sum((val - rates_) * -d_lambda1(term, x[5], x[2], x[3])),
    2 * sum((val - rates_) * -d_lambda2(term, x[6], x[4]))
  )
  v
}

gr_par <- function(x, val, term, n = 1) {
  bump <- 1e-5
  par_u <- x
  par_d <- x
  par_u[n] <- par_u[n] + bump
  par_d[n] <- par_d[n] - bump
  u <- f_obj(par_u, val, term)
  d <- f_obj(par_d, val, term)
  (u - d) / (2 * bump)
}

# check ----

beta0 <- as.numeric(fixedincome::last(sp_curve, "1 day"))
beta1 <- as.numeric(sp_curve[1]) - beta0
par <- c(beta0, beta1, 0.01, 0.01, 3, 0.5)

val_ <- as.numeric(sp_curve)
term_ <- as.numeric(toyears(sp_curve@daycount, sp_curve@terms))
x1 <- c(
  gr_par(par, val_, term_, 1),
  gr_par(par, val_, term_, 2),
  gr_par(par, val_, term_, 3),
  gr_par(par, val_, term_, 4),
  gr_par(par, val_, term_, 5),
  gr_par(par, val_, term_, 6)
)
x2 <- gr_f_obj(par,
  val = as.numeric(sp_curve),
  term = as.numeric(toyears(sp_curve@daycount, sp_curve@terms))
)
x1 - x2

# optim ----
beta0 <- as.numeric(fixedincome::last(sp_curve, "1 day"))
beta1 <- as.numeric(sp_curve[1]) - beta0
par <- c(beta0, beta1, 0.01, 0.01, 3, 1)
res2 <- optim(par,
  fn = f_obj,
  gr = gr_f_obj,
  lower = c(0,  -0.3, -1, -1,  1e-6, 1e-6),
  upper = c(0.3, 0.3,  1,  1,   5,  5),
  method = "L-BFGS-B",
  val = as.numeric(sp_curve),
  term = as.numeric(toyears(sp_curve@daycount, sp_curve@terms)),
  control = list(trace = 3, factr = 1e-10, maxit = 1000)
)
do.call(interp_nelsonsiegelsvensson, as.list(res2$par)) |> print()
interpolation(sp_curve) <- do.call(interp_nelsonsiegelsvensson, as.list(res2$par))
g <- ggplot2::autoplot(sp_curve, curve.geom = "point") +
  ggplot2::autolayer(sp_curve, curve.name = "nss", curve.interpolation = TRUE, size = 1)
print(g)

interpolation(sp_curve) |> parameters()
