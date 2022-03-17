
#' Shift vectors
#'
#' Element wise shift of vectors by `k` positions.
#'
#' @param x a vector object.
#' @param k a numeric with the number of elements to shift the Term vector
#' @param fill a numeric value (or \code{NA}) to fill the empty created by
#' shifting a vector object.
#' @param ... additional arguments. Currently unused.
#'
#' @return
#' A shifted vector object of the same type of provided object.
#'
#' @aliases shift,Term-method shift,numeric-method
#' @examples
#' shift(1:10, fill = 0)
#'
#' t <- term(1:10, "months")
#' shift(t)
#' @export
setGeneric(
  "shift",
  function(x, k = 1, ..., fill = NA) {
    standardGeneric("shift")
  }
)

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
      shifted <- c(x[seq_len(length(x) - k) + k], rep(fill, k))
    }

    shifted
  }
)

#' @export
plot.SpotRateCurve <- function(x, y, ...,
                               show_forward = FALSE,
                               use_interpolation = FALSE,
                               legend_location = "topright") {
  plot_SpotRateCurve(x, y, ...,
    show_forward = show_forward,
    use_interpolation = use_interpolation,
    legend_location = legend_location
  )
}

plot_SpotRateCurve <- function(x, y, ...,
                               show_forward = FALSE,
                               use_interpolation = FALSE,
                               legend_location = "topright") {
  title <- paste("SpotRateCurve", x@refdate)
  dc <- x@daycount
  X <- as.numeric(x@terms)
  Y <- as.numeric(x) * 100
  FWD <- as.numeric(forwardrate(x)) * 100
  family <- "mono"
  plot_op <- par(no.readonly = TRUE)
  on.exit(par(plot_op))
  par(family = family)

  rng <- if (show_forward) range(c(Y, FWD)) else range(Y)

  y_tick_inc <- 0.5
  spread <- diff(rng)
  y_ticks_q <- (ceiling(spread) * 1e2 + (y_tick_inc * 100)) / (y_tick_inc * 100)
  if (y_ticks_q %% 3 == 0) {
    y_ticks_q_min <- 6
  } else if (y_ticks_q %% 4 == 0) {
    y_ticks_q_min <- 8
  } else {
    y_ticks_q_min <- NA
  }
  y_ticks_n <- min(y_ticks_q, y_ticks_q_min)
  y_lower_lim <- (floor(rng[1] * 100 / (y_tick_inc * 100)) *
    (y_tick_inc * 100)) / 100
  y_upper_lim <- (ceiling(rng[2] * 100 / (y_tick_inc * 100)) *
    (y_tick_inc * 100)) / 100
  y_ticks <- if (!is.na(y_ticks_n)) {
    seq(y_lower_lim, y_upper_lim, length.out = y_ticks_n)
  } else {
    seq(y_lower_lim, y_upper_lim, y_tick_inc)
  }
  y_ticks_lab <- y_ticks |> format(digits = 4)

  x_tick_inc <- dib(dc)
  x_lower_lim <- 0
  x_upper_lim <- (as.integer(max(x@terms) / x_tick_inc) + 1) * x_tick_inc
  x_ticks <- seq(x_lower_lim, x_upper_lim, x_tick_inc)
  x_ticks_lab <- paste0(x_ticks / x_tick_inc, "Y")
  x_ticks[1] <- 1
  x_ticks_lab[1] <- "1D"

  plot(
    x = X, y = Y, type = "n", xlab = "Days", ylab = "%",
    ylim = c(y_lower_lim, y_upper_lim),
    # family = family,
    cex.main = 2,
    font.main = 2,
    cex.lab = 1,
    font.lab = 2,
    xaxt = "n",
    yaxt = "n",
    main = title
  )

  axis(1, x_ticks,
    labels = x_ticks_lab,
    # family = family,
    cex.axis = 0.8
  )
  axis(2, y_ticks,
    labels = y_ticks_lab,
    # family = family,
    cex.axis = 0.8
  )
  abline(h = y_ticks, v = x_ticks, lwd = 1, lty = 3, col = "lightgray")

  points(x = X, y = Y, pch = 19, cex = 1, col = "#e05305")
  legend_ <- list(list(lab = "Curve", col = "#e05305", pch = 19, lty = NA))

  if (use_interpolation) {
    rng <- range(x@terms)
    idx <- seq_len(rng[2])
    interp_curve <- x[[idx]]
    lines(
      x = as.numeric(interp_curve@terms),
      y = as.numeric(interp_curve) * 100,
      lwd = 2, col = "#4f7f81", lty = 2
    )
    legend_[[length(legend_) + 1]] <- list(
      lab = "Interpolation", col = "#4f7f81",
      pch = NA, lty = "dashed"
    )
    if (show_forward) {
      rng <- range(x@terms)
      idx <- seq_len(rng[2])
      interp_curve <- x[[idx]]
      fwd_rates <- forwardrate(interp_curve)
      X_ <- as.numeric(interp_curve@terms)
      Y_ <- as.numeric(fwd_rates) * 100
      lines(x = X_, y = Y_, lwd = 2, col = "#fbb407")
      legend_[[length(legend_) + 1]] <- list(
        lab = "Forward w/ Interpolation",
        col = "#fbb407",
        pch = NA, lty = "solid"
      )
    }
  } else {
    lines(x = X, y = Y, lwd = 2, col = "#e05305", lty = "dotted")
    if (show_forward) {
      sfun1 <- stepfun(x = X[-length(X)], y = FWD, right = TRUE)
      plot(sfun1, lwd = 2, pch = 19, col = "#fbb407", add = TRUE)
      legend_[[length(legend_) + 1]] <- list(
        lab = "Forward",
        col = "#fbb407",
        pch = 19, lty = "solid"
      )
    }
  }

  if (length(legend_) > 1) {
    leg_labs <- sapply(legend_, function(x) x$lab)
    leg_cols <- sapply(legend_, function(x) x$col)
    leg_pchs <- sapply(legend_, function(x) x$pch)
    leg_ltys <- sapply(legend_, function(x) x$lty)
    legend(legend_location,
      inset = 0.02, legend = leg_labs, col = leg_cols,
      pch = leg_pchs, lty = leg_ltys
    )
  }
}