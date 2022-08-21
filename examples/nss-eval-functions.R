
f1 <- function(t, l1) (1 - exp(-l1 * t)) / (l1 * t)

f1p <- function(t, l1) {
  -(1 / l1) * (1 - exp(-l1 * t) * (1 + l1 * t)) / (l1 * t)
}

f2 <- function(t, l1) ((1 - exp(-l1 * t)) / (l1 * t) - exp(-l1 * t))

term <- seq_len(15 * 252) / 252

beta2 <- - 0.014
plot(term,  beta2 * f1(term, 1), type = "n", ylim = sort(c(0, beta2)))
lines(term, beta2 * f1(term, 0.25), col = "grey80")
lines(term, beta2 * f1(term, 0.5), col = "grey")
lines(term, beta2 * f1(term, 1), col = "red")
lines(term, beta2 * f1(term, 2), col = "blue")
lines(term, beta2 * f1(term, 3), col = "magenta")

beta3 <- 0.117
plot(term,  beta3 * f2(term, 1), type = "n")
lines(term, beta3 * f2(term, 0.25), col = "grey80")
lines(term, beta3 * f2(term, 0.5), col = "grey")
lines(term, beta3 * f2(term, 1), col = "red")
lines(term, beta3 * f2(term, 2), col = "blue")
lines(term, beta3 * f2(term, 3), col = "magenta")

beta3 <- - 0.097
plot(term,  beta3 * f2(term, 1), type = "n")
lines(term, beta3 * f2(term, 0.25), col = "grey80")
lines(term, beta3 * f2(term, 0.5), col = "grey")
lines(term, beta3 * f2(term, 1), col = "red")
lines(term, beta3 * f2(term, 2), col = "blue")
lines(term, beta3 * f2(term, 3), col = "magenta")

# beta1   beta2   beta3   beta4 lambda1 lambda2
# 0.120  -0.014   0.117  -0.097   1.502   0.811

plot(term,  0.117 * f2(term, 1.5), type = "n", ylim = c(-0.04, 0.04))
lines(term, 0.117 * f2(term, 1.5), col = "red")
lines(term, -0.097 * f2(term, 0.8), col = "blue")
lines(term, 0.117 * f2(term, 1.5) -0.097 * f2(term, 0.8), col = "black")

d_f2 <- function(t, l1) {
  exp(l1 * t) - 1 - l1 * t - (l1 * t) ^ 2
}

plot(term[1:500], d_f2(term[1:500], 1.5), type = "l")
abline(h = 0, col = "red")

l1 <- 1.5
uniroot(d_f2, c(1e-6, l1 * 2), l1 = l1)
