source("C:/Users/wilso/Dev/R/R-fixedincome/examples/copom-functions.R")

# png("Rplot02.png",
#     res = 300,
#     height = 387,
#     width = 688)
curve <- get_curve_from_web("2022-02-23")
plot(curve)

# png("Rplot03.png",
#     res = 300,
#     height = 387,
#     width = 688)
curve <- get_curve_from_web("2011-02-23")
plot(curve)

# title <- paste("SpotRateCurve", curve@refdate)
# X <- as.numeric(curve@terms)
# Y <- as.numeric(curve) * 100
# family <- "mono"
# 
# y_tick_inc <- 0.5
# rng <- range(as.numeric(curve))*100
# spread <- diff(rng)
# y_ticks_q <- (ceiling(spread)*1e2 + (y_tick_inc*100))/(y_tick_inc*100)
# if (y_ticks_q %% 3 == 0) {
#   y_ticks_q_min <- 6
# } else if (y_ticks_q %% 4 == 0) {
#   y_ticks_q_min <- 8
# } else {
#   y_ticks_q_min <- NA
# }
# y_ticks_n <- min(y_ticks_q, y_ticks_q_min)
# y_lower_lim <- (floor(rng[1]*100 / (y_tick_inc*100)) * (y_tick_inc*100)) / 100
# y_upper_lim <- (ceiling(rng[2]*100 / (y_tick_inc*100)) * (y_tick_inc*100)) / 100
# y_ticks <- if (!is.na(y_ticks_n)) seq(y_lower_lim, y_upper_lim, length.out = y_ticks_n) else seq(y_lower_lim, y_upper_lim, y_tick_inc)
# y_ticks_lab <- y_ticks |> format(digits = 4)
# 
# x_tick_inc <- 252
# x_lower_lim <- 0
# x_upper_lim <- (as.integer(max(curve@terms) / x_tick_inc) + 1) * x_tick_inc
# x_ticks <- seq(x_lower_lim, x_upper_lim, x_tick_inc)
# x_ticks_lab <- paste0(x_ticks / x_tick_inc, "Y")
# x_ticks[1] <- 1
# x_ticks_lab[1] <- "1D"
# 
# plot(x = X, y = Y, type = 'n', xlab = "Days", ylab = "%",
#      ylim = c(y_lower_lim, y_upper_lim),
#      family = family,
#      cex.main = 2,
#      font.main = 2,
#      cex.lab = 1,
#      font.lab = 2,
#      xaxt = "n",
#      yaxt = "n",
#      main = title)
# axis(1, x_ticks, labels = x_ticks_lab, family = family, cex.axis = 0.8)
# axis(2, y_ticks, labels = y_ticks_lab, family = family, cex.axis = 0.8)
# 
# # grid(lwd = 1, ny = NA, nx = NULL)
# abline(h = y_ticks, v = x_ticks, lwd = 1, lty = 3, col = "lightgray")
# 
# points(x = X, y = Y, pch = 19, cex = 1, col = "#e05305")
# lines(x = X, y = Y, lwd = 2, col = "#e05305")
