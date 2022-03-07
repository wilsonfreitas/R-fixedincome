
source("examples/utils-functions.R")
curve <- get_curve_from_web("2022-03-04")
plot(first(curve, "2 years"), show_forward = TRUE)

first(curve, term(2, "years"))

closest(curve, "10 years")

last(curve, term(5, "years"))
