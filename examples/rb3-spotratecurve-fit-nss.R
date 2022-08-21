
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

# optim ----
beta0 <- as.numeric(fixedincome::last(sp_curve, "1 day"))
beta1 <- as.numeric(sp_curve[1]) - beta0
interp_ <- interp_nelsonsiegelsvensson(beta0, beta1, 0.01, 0.01, 2, 1)
interpolation(sp_curve) <- fit_interpolation(interp_, sp_curve)
interpolation(sp_curve)
g <- ggplot2::autoplot(sp_curve, curve.geom = "point") +
  ggplot2::autolayer(sp_curve, curve.name = "nss", curve.interpolation = TRUE, size = 1)
print(g)
