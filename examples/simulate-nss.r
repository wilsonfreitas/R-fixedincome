
source("examples/utils-functions.R")

di1 <- get_curve_from_web("2022-03-10")

beta0 <- as.numeric(last(di1, "1 day"))
beta1 <- as.numeric(di1[1]) - as.numeric(last(di1, "1 day"))
interpolation(di1) <- fit_interpolation(
  interp_nelsonsiegelsvensson(beta0, beta1, 0.01, 0.01, 2 / 252, 1 / 1008),
  di1
)
plot(di1, use_interpolation = TRUE)
di1@interpolation

simulate_interpolation <- function(interp, ...) {
  params <- unlist(list(...))
}

i1 <- interp
plot(1:2500, interpolate(i1, 1:2500), type = "l")
parameters(i1) <- c(lambda1 = 0.002)
lines(1:2500, interpolate(i1, 1:2500), col = "grey")