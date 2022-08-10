
library(rb3)
library(bizdays)
library(fixedincome)
library(copom)
library(tidyverse)

get_di1_curve <- function(refdate) {
  fut <- futures_get(refdate)
  yc <- yc_get(refdate)
  df <- yc_superset(yc, fut)

  df_curve <- bind_rows(
    df |> slice(1) |> select(biz_days, r_252),
    df |> filter(!is.na(symbol)) |> select(biz_days, r_252)
  ) |>
    filter(!duplicated(biz_days))

  spotratecurve(
    df_curve$r_252, df_curve$biz_days, "discrete", "business/252", "Brazil/ANBIMA",
    refdate = refdate
  )
}


refdate <- getdate("last bizday", Sys.Date(), "Brazil/ANBIMA")
curve <- get_di1_curve(refdate)
curve2 <- get_di1_curve(refdate - 1)

copom_dates <- get_copom_dates(refdate, 8)
interpolation(curve) <- interp_flatforwardcopom(copom_dates, "second")
ggspotratecurveplot(curve, title = "AA", subtitle = "BB", caption = "WW")

ggspotratecurveplot(curve,
  title = "AA", subtitle = "BB", caption = "WW",
  curve.x.axis = "dates"
)

g <- ggspotratecurveplot(curve, curve.x.axis = "dates")

g +
  autolayer(curve2, curve.x.axis = "dates", size = 1) +
  autolayer(curve2, curve.x.axis = "dates", curve.geom = "point", size = 2)

autoplot(curve, size = 1, curve.interpolation = TRUE) +
  autolayer(curve, curve.name = "no interp")

autoplot(curve |> fixedincome::first("2 years"), size = 1) +
  autolayer(forwardrate(curve |> fixedincome::first("2 years")), size = 1) +
  theme_wf()

autoplot(curve, size = 1) +
  autolayer(curve2, size = 1) +
  scale_colour_manual("", values = sample(colors(), 2)) +
  theme_wf()

autoplot(curve, size = 1) +
  autolayer(curve, curve.geom = "point", size = 2) +
  autolayer(curve2, size = 1) +
  autolayer(curve2, curve.geom = "point", size = 2) +
  scale_colour_manual("", values = sample(colors(), 2)) +
  theme_wf()

colours <- sample(colors(), 2)
names(colours) <- sample(letters, 2)
autoplot(curve, curve.name = names(colours)[1], size = 1) +
  autolayer(curve2, curve.name = names(colours)[2], size = 1) +
  scale_colour_manual("", breaks = names(colours), values = colours) +
  theme_wf()

# ----

curve_fwd <- forwardrate(curve)
curve_fwd <- spotratecurve(
  as.numeric(curve_fwd),
  cumsum(curve_fwd@terms),
  refdate = curve@refdate,
  .copyfrom = curve
) |> as.data.frame()
curve_spt <- as.data.frame(curve)

.dash <- "#4f7f81"
.colors <- c("#e05305", "#fbb407")
.names <- c("Curva Zero", "Curva Forward")
.nidx <- 1
names(.colors) <- .names

g <- ggplot()

if (!is.null(copom_dates)) {
  g <- g + geom_vline(
    xintercept = copom_dates, colour = .dash,
    linetype = "dashed", size = 1
  )
}

g <- g +
  geom_line(
    data = curve_spt,
    mapping = aes(x = dates, y = rates, colour = .names[1]),
    size = 1
  ) +
  geom_point(
    data = curve_spt,
    mapping = aes(x = dates, y = rates, colour = .names[1]),
    size = 2
  )

if (show_forward) {
  g <- g +
    geom_step(
      data = curve_fwd,
      mapping = aes(x = dates, y = rates, colour = .names[2]),
      size = 1,
      direction = "vh"
    ) +
    geom_point(
      data = curve_fwd,
      mapping = aes(x = dates, y = rates, colour = .names[2]),
      size = 2
    )
  .nidx <- seq_along(.names)
}

g <- g +
  scale_colour_manual("", breaks = .names[.nidx], values = .colors[.nidx])

.title <- glue("Curva de Juros Prefixados DI1 - {refdate}",
  refdate = format(curve@refdate)
)

.subtitle <- if (is.null(copom_dates)) {
  NULL
} else {
  "As linhas cinza tracejadas representam as datas do COPOM"
}

g <- g +
  labs(
    x = "Data",
    y = NULL,
    title = .title,
    # subtitle = .subtitle,
    caption = "Desenvolvido por wilsonfreitas (com dados da B3)"
  ) +
  theme_wf(base_size = 16) +
  scale_y_continuous(labels = scales::percent)
g
