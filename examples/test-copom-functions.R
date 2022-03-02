
source("examples/utils-functions.R")

di1 <- get_curve_from_web("2022-02-23")

rng <- range(offset(di1@refdate, di1@terms, di1@calendar))
copomDates <- offset(copom_dates, 1, di1@calendar)
ix <- copomDates >= rng[1] & copomDates <= rng[2]
copomDates <- copomDates[ix] # [1:4]

interpolation(di1) <- interp_flatforwardcopom(copomDates, "second")
plot(di1[1:10], use_interpolation = TRUE, show_forward = TRUE, legend_location = "bottom")
plot(di1[1:10], use_interpolation = TRUE, legend_location = "bottom")

di1ff <- di1
interpolation(di1ff) <- interp_flatforward()
plot(di1ff[1:10], use_interpolation = TRUE, legend_location = "bottom")

parts <- split_curve_into_copom_dates(di1, copomDates)

# res0 <- calc_first_term(parts, 1, NULL)
# res1 <- calc_with_first_future(parts, 2, list(res0))
# res2 <- calc_with_optim(parts, 3, list(res0, res1))
# res2 <- calc_with_forward(parts, 3, list(res0, res1))
# res2 <- calc_with_first_future(parts, 3, list(res0, res1))
# 
# res <- copom_calc(parts, 1, conflicts = "forward")
# copom_calc(parts, 1, conflicts = "first")
idx <- di1@terms <= 504
copom_calc(parts, 1, conflicts = "first")
copom_calc(parts, 1, conflicts = "second")
res <- copom_calc(parts, 1, conflicts = "second")
plot_copom_curve(di1[idx], res, copomDates)

zero_copom <- spotratecurve(
  res$rates_zero,
  res$terms,
  refdate = di1@refdate
)

zero_copom <- c(di1[1], zero_copom, di1[di1@terms > max(zero_copom@terms)])
interpolation(zero_copom) <- interp_flatforward()

plot(zero_copom[1:20], use_interpolation = TRUE, show_forward = TRUE)


as.data.frame(zero_copom)


x <- copom_calc(parts, 1, conflicts = "forward")

di1 <- get_curve_from_web("2022-02-18")

idx <- di1@terms <= 504
plot_curve(di1[idx], copomDates)

res <- copom_calc(parts, 1, conflicts = "forward")
idx <- di1@terms <= 252
plot_copom_curve(di1[idx], res, copomDates)
