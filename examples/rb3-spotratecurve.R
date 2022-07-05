
library(rb3)
library(bizdays)
library(tidyverse)
# library(fixedincome)
library(nloptr)

refdate <- getdate("last bizday", Sys.Date(), "Brazil/ANBIMA")
yc <- yc_get(refdate)
fut <- futures_get(refdate)
yc_ss <- yc_superset(yc, fut)
yc_ss_first <- yc_ss |> slice(1)
yc_ss_curve <- yc_ss |> filter(!is.na(symbol))
yc_ <- bind_rows(yc_ss_first, yc_ss_curve)

sp_curve <- spotratecurve(
  yc_$r_252, yc_$biz_days,
  "discrete", "business/252", "Brazil/ANBIMA",
  refdate = refdate
)

plot(sp_curve)

f_obj <- function(x, val, term) {
  rates_ <- fixedincome:::nss(term, x[1], x[2], x[3], x[4], x[5], x[6])
  sqrt(sum((val - rates_)^2))
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
    sum((val - rates_) * -d_beta1(term)),
    sum((val - rates_) * -d_beta2(term, x[5])),
    sum((val - rates_) * -d_beta3(term, x[5])),
    sum((val - rates_) * -d_beta3(term, x[6])),
    sum((val - rates_) * -d_lambda1(term, x[5], x[2], x[3])),
    sum((val - rates_) * -d_lambda2(term, x[6], x[4]))
  )
  v / obj
}

g_obj <- function(x, val, term) {
  c(
    -x[1],
    -x[1] - x[2],
    -x[5],
    -x[6],
    -x[5] + x[6]
  )
}

gr_g_obj <- function(x, val, term) {
  rbind(
    c(-1,  0, 0, 0, 0, 0),
    c(-1, -1, 0, 0, 0, 0),
    c(0, 0, 0, 0, -1, 0),
    c(0, 0, 0, 0, 0, -1),
    c(0, 0, 0, 0, -1, 1)
  )
}

gr_par(par,
  val = as.numeric(sp_curve), term = toyears(sp_curve@daycount, sp_curve@terms),
  n = 6
)
gr_obj(par, val = as.numeric(sp_curve), term = toyears(sp_curve@daycount, sp_curve@terms))

# interp <- interp_nelsonsiegelsvensson(beta0, beta1, 0.01, 0.01, 2 / 252, 1 / 1008)
# interpolation(sp_curve) <- fit_interpolation(interp, sp_curve)
# plot(sp_curve, use_interpolation = TRUE)
# sp_curve@interpolation

# NLOPT_GN_ISRES ----
beta0 <- as.numeric(fixedincome::last(sp_curve, "1 day"))
beta1 <- as.numeric(sp_curve[1]) - beta0
par <- c(beta0, beta1, 0.01, 0.01, 3, 0.5)
opts <- list(
  "algorithm"   = "NLOPT_GN_ISRES",
  "xtol_rel"    = 1.0e-16,
  "maxeval"     = 500000,
  "print_level" = 0
)
res <- nloptr(par,
  eval_f = f_obj,
  # eval_grad_f = gr_obj,
  eval_g_ineq = g_obj,
  # eval_jac_g_ineq = gr_g_obj,
  lb = c(0,  -0.3, -1, -1,  1e-6, 1e-6),
  ub = c(0.3, 0.3,  1,  1,   5,  3),
  opts = opts,
  val = as.numeric(sp_curve),
  term = toyears(sp_curve@daycount, sp_curve@terms)
)
do.call(interp_nelsonsiegelsvensson, as.list(res$solution))

interpolation(sp_curve) <- do.call(interp_nelsonsiegelsvensson, as.list(res$solution))
sp_curve |> plot(use_interpolation = TRUE)

# NLOPT_LD ----
par <- res$solution # c(beta0, beta1, 0.01, 0.01, 3, 0.5)
opts <- list(
  "algorithm"   = "NLOPT_LD_MMA",
  "xtol_rel"    = 1.0e-16,
  "maxeval"     = 500000,
  "print_level" = 0
)
res1 <- nloptr(par,
  eval_f = f_obj,
  eval_grad_f = gr_f_obj,
  eval_g_ineq = g_obj,
  eval_jac_g_ineq = gr_g_obj,
  lb = c(0,  -0.3, -1, -1,  1e-6, 1e-6),
  ub = c(0.3, 0.3,  1,  1,   5,  3),
  opts = opts,
  val = as.numeric(sp_curve),
  term = toyears(sp_curve@daycount, sp_curve@terms)
)
do.call(interp_nelsonsiegelsvensson, as.list(res1$solution))

interpolation(sp_curve) <- do.call(interp_nelsonsiegelsvensson, as.list(res1$solution))
sp_curve |> plot(use_interpolation = TRUE)

# optim ----
par <- res$solution # c(beta0, beta1, 0.01, 0.01, 3, 0.5)
res2 <- optim(par,
  fn = f_obj,
  gr = gr_f_obj,
  lower = c(0,  -0.3, -1, -1,  1e-6, 1e-6),
  upper = c(0.3, 0.3,  1,  1,   5,  3),
  method = "L-BFGS-B",
  val = as.numeric(sp_curve),
  term = toyears(sp_curve@daycount, sp_curve@terms)
)
do.call(interp_nelsonsiegelsvensson, as.list(res2$par))

interpolation(sp_curve) <- do.call(interp_nelsonsiegelsvensson, as.list(res2$par))
sp_curve |> plot(use_interpolation = TRUE)

sp_curve1 <- sp_curve |> fixedincome::first("3 years")
interpolation(sp_curve1) <- do.call(interp_nelsonsiegelsvensson, as.list(res2$par))
sp_curve1 |> plot(use_interpolation = TRUE)