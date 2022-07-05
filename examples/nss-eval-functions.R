
f1 <- function(t, l1) (1 - exp(-l1 * t)) / (l1 * t)

f1p <- function(t, l1) {
  -(1 / l1) * (1 - exp(-l1 * t) * (1 + l1 * t)) / (l1 * t)
}

f2 <- function(t, l1) ((1 - exp(-l1 * t)) / (l1 * t) - exp(-l1 * t))

term <- seq_len(15 * 252) / 252

beta2 <- 0.02
plot(term,  1 * f1(term, 1), type = "n", ylim = c(0, 1))
lines(term, beta2 * f1(term, 0.25), col = "grey80")
lines(term, beta2 * f1(term, 0.5), col = "grey")
lines(term, beta2 * f1(term, 1), col = "red")
lines(term, beta2 * f1(term, 2), col = "blue")
lines(term, beta2 * f1(term, 3), col = "magenta")

beta3 <- 0.1766474
plot(term,  beta3 * f1(term, 1), type = "n")
lines(term, beta3 * f2(term, 0.25), col = "grey80")
lines(term, beta3 * f2(term, 0.5), col = "grey")
lines(term, beta3 * f2(term, 1), col = "red")
lines(term, beta3 * f2(term, 2), col = "blue")
lines(term, beta3 * f2(term, 3), col = "magenta")
lines(term, beta3 * f2(term, 1.440675), col = "black")

beta3 <- -0.1689549
plot(term,  beta3 * f1(term, 1), type = "n")
lines(term, beta3 * f2(term, 0.25), col = "grey80")
lines(term, beta3 * f2(term, 0.5), col = "grey")
lines(term, beta3 * f2(term, 1), col = "red")
lines(term, beta3 * f2(term, 2), col = "blue")
lines(term, beta3 * f2(term, 3), col = "magenta")
lines(term, beta3 * f2(term, 1.105058), col = "black")