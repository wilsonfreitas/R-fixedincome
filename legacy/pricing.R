bsm.call <- function(S, K, r, t, sig, b) {
    d1 <- (log(S/K) + (b + (sig^2)/2)*t) / (sig*sqrt(t))
    d2 <- d1 - sig*sqrt(t)
    
    return(S*exp((b - r)*t)*pnorm(d1) - K*exp(-r*t)*pnorm(d2))
}

sig <- 0.00514191055371176
price <- 291.749820458339
mat <- '2014-07-01'
delta <- 0.5
# 2014-07-01    322	221	9.480
du <- 221
r <- log(1 + 0.0948)
S <-  164924.5272191
K <- 164926.439265986
bsm.call(S, K, r, du/252, sig, 0) - price

