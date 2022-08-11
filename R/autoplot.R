#' ggplot2 plotting functions
#'
#' Functions to plot fixedincome objects using ggplot2 package and its grammar
#' of graphics.
#'
#' @name ggplot2-support
#'
#' @param object SpotRateCurve or ForwardRate objects
#' @param curve.name Curve's name
#' @param curve.geom Curve geom used: `line`, `point`, `step`
#' @param curve.interpolation logical indicating to use curve interpolation in
#' the plot. Defaults to `FALSE`.
#' @param curve.x.axis x axis can be presented with a numeric scale representing
#' business days (`terms`) or dates (`dates`). Defaults to `dates`.
#' @param ... additional arguments passed to ggplot2 geom_* functions
#'
NULL

#' @rdname ggplot2-support
#' @export
autoplot.SpotRateCurve <- function(object, ...,
                                   curve.name = NULL,
                                   curve.geom = c("line", "point"),
                                   curve.interpolation = FALSE,
                                   curve.x.axis = c("dates", "terms")) {
  curve.geom <- match.arg(curve.geom)
  curve.x.axis <- match.arg(curve.x.axis)
  curve <- object
  if (curve.interpolation && is.null(interpolation(curve))) {
    stop("Curve does not have interpolation")
  }
  if (curve.interpolation) {
    terms_mm <- range(as.numeric(curve@terms))
    curve_it <- curve[[seq(terms_mm[1], terms_mm[2])]]
    curve_spt <- as.data.frame(curve_it)
    curve_spt[["terms"]] <- as.numeric(curve_it@terms)
    curve_spt[["rates"]] <- as.numeric(curve_spt[["rates"]])
  } else {
    curve_spt <- as.data.frame(curve)
    curve_spt[["terms"]] <- as.numeric(curve_spt[["terms"]])
    curve_spt[["rates"]] <- as.numeric(curve_spt[["rates"]])
  }
  if (is.null(curve.name)) {
    curve.name <- format(curve@refdate)
  }
  curve_spt[["curve.name"]] <- curve.name
  if (curve.x.axis == "terms") {
    curve_spt[["x"]] <- curve_spt[["terms"]]
  } else if (curve.x.axis == "dates") {
    curve_spt[["x"]] <- curve_spt[["dates"]]
  }
  p <- ggplot(
    data = curve_spt,
    mapping = aes_string(x = "x", y = "rates", colour = "curve.name")
  )
  pp <- if (curve.geom == "line") {
    geom_line(...)
  } else if (curve.geom == "point") {
    geom_point(...)
  }
  p + pp
}

#' @rdname ggplot2-support
#' @export
autolayer.SpotRateCurve <- function(object, ...,
                                    curve.name = NULL,
                                    curve.geom = c("line", "point"),
                                    curve.interpolation = FALSE,
                                    curve.x.axis = c("dates", "terms")) {
  curve.geom <- match.arg(curve.geom)
  curve.x.axis <- match.arg(curve.x.axis)
  curve <- object
  if (curve.interpolation && is.null(interpolation(curve))) {
    stop("Curve does not have interpolation")
  }
  curve_spt <- as.data.frame(curve)
  if (curve.interpolation) {
    terms_mm <- range(as.numeric(curve@terms))
    curve_it <- curve[[seq(terms_mm[1], terms_mm[2])]]
    curve_spt <- as.data.frame(curve_it)
    curve_spt[["terms"]] <- as.numeric(curve_it@terms)
    curve_spt[["rates"]] <- as.numeric(curve_spt[["rates"]])
  } else {
    curve_spt <- as.data.frame(curve)
    curve_spt[["terms"]] <- as.numeric(curve_spt[["terms"]])
    curve_spt[["rates"]] <- as.numeric(curve_spt[["rates"]])
  }
  if (is.null(curve.name)) {
    curve.name <- format(curve@refdate)
  }
  curve_spt[["curve.name"]] <- curve.name
  if (curve.x.axis == "terms") {
    curve_spt[["x"]] <- curve_spt[["terms"]]
  } else if (curve.x.axis == "dates") {
    curve_spt[["x"]] <- curve_spt[["dates"]]
  }
  if (curve.geom == "line") {
    geom_line(
      data = curve_spt, mapping = aes_string(x = "x", y = "rates", colour = "curve.name"), ...
    )
  } else if (curve.geom == "point") {
    geom_point(
      data = curve_spt, mapping = aes_string(x = "x", y = "rates", colour = "curve.name"), ...
    )
  }
}

#' @rdname ggplot2-support
#' @export
autolayer.ForwardRate <- function(object, ...,
                                  curve.name = NULL,
                                  curve.geom = c("step", "line", "point"),
                                  curve.x.axis = c("dates", "terms")) {
  curve.geom <- match.arg(curve.geom)
  curve.x.axis <- match.arg(curve.x.axis)
  curve_fwd <- object
  df <- as.data.frame(object)
  df[["terms"]] <- as.numeric(df[["terms"]])
  df[["rates"]] <- as.numeric(df[["rates"]])
  if (is.null(curve.name)) {
    curve.name <- "Forward Rate"
  }
  df[["curve.name"]] <- curve.name
  if (curve.x.axis == "terms") {
    df[["x"]] <- df[["terms"]]
  } else if (curve.x.axis == "dates") {
    df[["x"]] <- df[["dates"]]
  }
  if (curve.geom == "line") {
    geom_line(
      data = df,
      mapping = aes_string(x = "x", y = "rates", colour = "curve.name"),
      ...
    )
  } else if (curve.geom == "step") {
    geom_step(
      data = df,
      mapping = aes_string(x = "x", y = "rates", colour = "curve.name"),
      direction = "vh",
      ...
    )
  } else if (curve.geom == "point") {
    geom_point(
      data = df,
      mapping = aes_string(x = "x", y = "rates", colour = "curve.name"),
      ...
    )
  }
}

#' Fancy ggplot for SpotRateCurve object
#'
#' Fancy ggplot for SpotRateCurve object with custom axis, title
#'
#' @param curve SpotRateCurve object
#' @param title plot title
#' @param subtitle plot subtitle
#' @param caption plot caption
#' @param curve.name Curve's name, if not provided curve's refdate is used.
#' @param curve.x.axis x axis can be presented with a numeric scale representing
#' business days (`terms`) or dates (`dates`). Defaults to `dates`.
#' @param ... additional arguments (not used)
#'
#' @importFrom scales percent
#' @export
#' @examples
#' \dontrun{
#' terms <- c(1, 11, 26, 27, 28)
#' rates <- c(0.0719, 0.056, 0.0674, 0.0687, 0.07)
#' curve <- spotratecurve(rates, terms, "discrete", "actual/365", "actual")
#' ggspotratecurveplot(curve)
#' }
ggspotratecurveplot <- function(curve,
                                title = NULL,
                                subtitle = NULL,
                                caption = NULL,
                                curve.name = NULL,
                                curve.x.axis = c("dates", "terms"), ...) {
  curve.x.axis <- match.arg(curve.x.axis)
  autoplot(curve,
    curve.name = curve.name, curve.x.axis = curve.x.axis, size = 1
  ) +
    autolayer(curve,
      curve.name = curve.name, curve.x.axis = curve.x.axis, curve.geom = "point",
      size = 2
    ) +
    theme_wf() +
    theme(legend.title = element_blank()) +
    labs(
      y = NULL, x = "Tenor (Business Days)",
      title = title, subtitle = subtitle, caption = caption
    ) +
    scale_y_continuous(labels = percent)
}

theme_wf <- function(base_size = 12,
                     base_family = "mono",
                     base_line_size = base_size / 22,
                     base_rect_size = base_size / 22) {
  theme_grey(
    base_size = base_size,
    base_family = base_family,
    base_line_size = base_line_size,
    base_rect_size = base_rect_size
  ) %+replace%
    theme(
      legend.position = "top",
      text = element_text(family = base_family, size = base_size),
      plot.background = element_blank(),
      panel.background = element_blank(),
      strip.background = element_blank(),
      panel.grid = element_line(colour = "grey92"),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      title = element_text(
        family = base_family,
        colour = "black",
        face = "bold"
      ),
      axis.line = element_line(colour = "grey92", size = 1),
      axis.title.y = element_text(colour = "black", face = "bold"),
      axis.title.x = element_text(colour = "black", face = "bold"),
      legend.key = element_rect(fill = "white", colour = NA),
      complete = TRUE
    )
}
