
#' @export
setGeneric(
  "shift",
  function(x, k = 1, ...) {
    standardGeneric("shift")
  }
)

#' @export
setMethod(
  "shift",
  signature(x = "numeric"),
  function(x, k = 1, ..., fill = NA) {
    stopifnot(length(x) >= k)
    if (k == 0) {
      return(x)
    }
    
    forward <- k > 0
    k <- abs(k)
    
    if (forward) {
      shifted <- c(rep(fill, k), x[seq_len(length(x) - k)])
    } else {
      shifted <- c(x[seq_len(length(x)-k)+k], rep(fill, k))
    }
    
    shifted
  }
)

#' @export
plot.SpotRateCurve <- function(x, y, ...) {
  title <- paste("SpotRateCurve", x@refdate)
  X <- as.numeric(x@terms)
  Y <- as.numeric(x) * 100
  family <- "mono"
  
  y_tick_inc <- 0.5
  rng <- range(Y)
  spread <- diff(rng)
  y_ticks_q <- (ceiling(spread)*1e2 + (y_tick_inc*100))/(y_tick_inc*100)
  if (y_ticks_q %% 3 == 0) {
    y_ticks_q_min <- 6
  } else if (y_ticks_q %% 4 == 0) {
    y_ticks_q_min <- 8
  } else {
    y_ticks_q_min <- NA
  }
  y_ticks_n <- min(y_ticks_q, y_ticks_q_min)
  y_lower_lim <- (floor(rng[1]*100 / (y_tick_inc*100)) * (y_tick_inc*100)) / 100
  y_upper_lim <- (ceiling(rng[2]*100 / (y_tick_inc*100)) * (y_tick_inc*100)) / 100
  y_ticks <- if (!is.na(y_ticks_n)) seq(y_lower_lim, y_upper_lim, length.out = y_ticks_n) else seq(y_lower_lim, y_upper_lim, y_tick_inc)
  y_ticks_lab <- y_ticks |> format(digits = 4)
  
  x_tick_inc <- 252
  x_lower_lim <- 0
  x_upper_lim <- (as.integer(max(x@terms) / x_tick_inc) + 1) * x_tick_inc
  x_ticks <- seq(x_lower_lim, x_upper_lim, x_tick_inc)
  x_ticks_lab <- paste0(x_ticks / x_tick_inc, "Y")
  x_ticks[1] <- 1
  x_ticks_lab[1] <- "1D"
  
  plot(x = X, y = Y, type = 'n', xlab = "Days", ylab = "%",
       ylim = c(y_lower_lim, y_upper_lim),
       family = family,
       cex.main = 2,
       font.main = 2,
       cex.lab = 1,
       font.lab = 2,
       xaxt = "n",
       yaxt = "n",
       main = title)
  axis(1, x_ticks, labels = x_ticks_lab, family = family, cex.axis = 0.8)
  axis(2, y_ticks, labels = y_ticks_lab, family = family, cex.axis = 0.8)
  abline(h = y_ticks, v = x_ticks, lwd = 1, lty = 3, col = "lightgray")
  points(x = X, y = Y, pch = 19, cex = 1, col = "#e05305")
  lines(x = X, y = Y, lwd = 2, col = "#e05305")
}
