
di1 <- get_curve_from_web("2022-02-18")



rng <- range(offset(di1@refdate, di1@terms, di1@calendar))
copomDates <- offset(copom_dates, 1, di1@calendar)
ix <- copomDates >= rng[1] & copomDates <= rng[2]
copomDates <- copomDates[ix][1:4]

parts <- split_curve_into_copom_dates(di1, copomDates)

res0 <- calc_first_term(parts, 1, NULL)
res1 <- calc_with_first_future(parts, 2, list(res0))
res2 <- calc_with_optim(parts, 3, list(res0, res1))
res2 <- calc_with_forward(parts, 3, list(res0, res1))
res2 <- calc_with_first_future(parts, 3, list(res0, res1))

res <- copom_calc(parts, 1, conflicts = "forward")
copom_calc(parts, 1, conflicts = "first")
copom_calc(parts, 1, conflicts = "optim")

fwd_curve <- forwardratecurve(res0$zero_curve)
append(fwd_curve, res0$copom_forward)

x <- copom_calc(parts, 1, conflicts = "forward")

di1 <- get_curve_from_web("2022-02-18")

idx <- di1@terms <= 504
plot_curve(di1[idx], copomDates)

res <- copom_calc(parts, 1, conflicts = "forward")
idx <- di1@terms <= 252
plot_copom_curve(di1[idx], res, copomDates)
