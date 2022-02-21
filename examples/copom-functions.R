
library(glue)
library(xml2)
library(stringr)
library(httr)
library(ggplot2)
library(dplyr)
library(bizdays)
library(forcats)
library(rbcb)
library(fixedincome)


copom_dates <- c("02/02/2022", "16/03/2022", "04/05/2022", "15/06/2022",
                 "03/08/2022", "21/09/2022", "26/10/2022", "07/12/2022",
                 "20/01/2021", "17/03/2021", "05/05/2021", "16/06/2021",
                 "04/08/2021", "22/09/2021", "27/10/2021", "08/12/2021",
                 "05/02/2020", "18/03/2020", "06/05/2020", "17/06/2020",
                 "05/08/2020", "16/09/2020", "28/10/2020", "09/12/2020",
                 "06/02/2019", "20/03/2019", "08/05/2019", "19/06/2019",
                 "31/07/2019", "18/09/2019", "30/10/2019", "11/12/2019",
                 "07/02/2018", "21/03/2018", "16/05/2018", "20/06/2018",
                 "01/08/2018", "19/09/2018", "31/10/2018", "12/12/2018",
                 "11/01/2017", "22/02/2017", "12/04/2017", "31/05/2017",
                 "26/07/2017", "06/09/2017", "25/10/2017", "06/12/2017",
                 "20/01/2016", "02/03/2016", "27/04/2016", "08/06/2016",
                 "20/07/2016", "31/08/2016", "19/10/2016", "29/11/2016",
                 "21/01/2015", "04/03/2015", "29/04/2015", "03/06/2015",
                 "29/07/2015", "02/09/2015", "21/10/2015", "25/11/2015") |>
  as.Date("%d/%m/%Y") |> 
  sort()

# functions ----

split_curve_into_copom_dates <- function(curve, copom_dates) {
  curve_fwd <- forwardrate(curve)
  dates_terms <- maturities(curve)
  lapply(seq_along(copom_dates), function(x) {
    if (x == 1) {
      idx <- c(1, which(dates_terms >= copom_dates[x])[1])
    } else if (!is.na(copom_dates[x+1])) {
      idx <- which(dates_terms >= copom_dates[x] & dates_terms <= copom_dates[x+1])
    } else {
      idx <- which(dates_terms >= copom_dates[x])
    }
    list(
      copom_date = copom_dates[x],
      futures = curve[idx],
      forward = curve_fwd[idx]
    )
  })
}

calc_zero <- function(last_result, du_copom, futs) {
  # this is the forward rate that starts at the last copom date
  fwd_copom <- last_result$copom_forward
  # replace the terms to extend it up to the next copom date
  fwd_copom@terms <- du_copom - last_result$zero@terms
  fwd_rates <- forwardrate(last_result$zero)
  # compose the zero with the rates up to the last copom date and
  # from the last to the next copom date
  fwd_rates <- c(fwd_rates, fwd_copom)
  spot_curve <- as.spotratecurve(fwd_rates, last_result$zero@refdate)
  spot_curve[[du_copom]]
}

calc_zero_use_first_future <- function(last_result, du_copom, futs) {
  spotratecurve(
    as.numeric(futs[1]),
    du_copom,
    refdate = futs@refdate,
    .copyfrom = futs
  )
}

calc_first_term <- function(parts, x, results) {
  copom_date <- parts[[x]]$copom_date
  futs <- parts[[x]]$futures
  fwds <- parts[[x]]$forward
  refdate <- futs@refdate
  du_copom <- term(bizdays(refdate, copom_date, "Brazil/ANBIMA"))
  
  du_rem <- futs@terms[length(futs@terms)] - du_copom
  
  if (du_rem > 0) {
    zero <- spotratecurve(
      as.numeric(futs[1]),
      du_copom,
      refdate = futs@refdate,
      .copyfrom = futs
    )
    futs[[du_copom]] <- zero
    fwd <- forwardrate(futs, du_copom, futs@terms[length(futs@terms)])
    copom_curve_term(
      copom_date = copom_date,
      zero = zero,
      copom_forward = fwd,
      move = 1e4 * (as.numeric(fwd) - as.numeric(futs[1]))
    )
  } else {
    fwd <- 10.65/100
    copom_curve_term(
      copom_date = copom_date,
      zero = as.numeric(futs[1]),
      copom_forward = fwd,
      move = 1e4 * (fwd - as.numeric(futs[1]))
    )
  }
}

calc_with <- function(parts, x, results, zero_calc, forward_calc) {
  copom_date <- parts[[x]]$copom_date
  futs <- parts[[x]]$futures
  fwds <- parts[[x]]$forward
  refdate <- futs@refdate
  du_copom <- term(bizdays(refdate, copom_date, "Brazil/ANBIMA"))
  
  last_result <- results[[length(results)]]
  
  zero <- zero_calc(last_result, du_copom, futs)
  fwd <- forward_calc(futs, fwds, du_copom, zero)
  move <- if (is.null(last_result)) {
    1e4 * (as.numeric(fwd) - as.numeric(futs[1]))
  } else {
    1e4 * (as.numeric(fwd) - as.numeric(last_result$copom_forward))
  }
  
  copom_curve_term(
    copom_date = copom_date,
    zero = zero,
    copom_forward = fwd,
    move = move
  )
}

forward_calc_use_first_future <- function(futs, fwds, du_copom, zero) {
  futs[[du_copom]] <- zero
  idx <- match(du_copom, futs@terms)
  forwardrate(futs, du_copom, futs@terms[idx+1])
}

forward_calc_use_forward <- function(futs, fwds, du_copom, zero) {
  fwds[2]
}

forward_calc_optim <- function(futs, fwds, du_copom, zero) {
  du_fwd <- futs@terms - du_copom
  f_obj <- function(x) {
    spot_rate <- spotratecurve(rep(x, length(du_fwd)), du_fwd,
                               refdate = zero@refdate,
                               .copyfrom = futs)
    fact_obj <- compound(zero) * compound(spot_rate)
    sum(compound(futs) - fact_obj) ^ 2
  }
  res <- optim(as.numeric(fwds[2]), f_obj, method = "Brent",
               lower = 0, upper = 1)
  forwardrate(res$par, du_fwd, .copyfrom = futs)
}

copom_calc <- function(parts, x = 1, results = NULL,
                       conflicts = c("forward", "first", "optimize")) {
  
  if (x > length(parts)) {
    moves <- do.call(c, lapply(results, function(x) x$move))
    zero_curve <- do.call(c, lapply(results, function(x) x$zero))
    forward_rates <- do.call(c, lapply(results, function(x) x$copom_forward))
    forward_curve <- spotratecurve(
      as.numeric(forward_rates),
      zero_curve@terms,
      .copyfrom = zero_curve,
      refdate = zero_curve@refdate
    )
    
    zero_df <- zero_curve |> as.data.frame()
    forward_df <- forward_curve |> as.data.frame()
    
    df <- merge(zero_df, forward_df,
                by = c("terms", "dates"),
                suffixes = c("_zero", "_forward")) |>
      arrange(terms) |>
      mutate(move = moves)
    
    return(df)
  }
  
  if (x == 1) {
    # result <- calc_first_term(parts, x, results)
    result <- calc_with(parts, x, results, calc_zero_use_first_future, forward_calc_use_first_future)
    results[[length(results) + 1]] <- result
    copom_calc(parts, x+1, results, conflicts)
  } else {
    conflicts <- match.arg(conflicts)
    futs <- parts[[x]]$futures
    result <- if (length(futs) == 2) {
      if (conflicts == "forward") {
        calc_with(parts, x, results, calc_zero, forward_calc_use_forward)
      } else if (conflicts == "first") {
        calc_with(parts, x, results, calc_zero, forward_calc_use_first_future)
      } else if (conflicts == "optimize") {
        calc_with(parts, x, results, calc_zero, forward_calc_optim)
      }
    } else if (length(futs) == 0) {
      NULL
    } else {
      calc_with(parts, x, results, calc_zero, forward_calc_use_first_future)
    }
    results[[length(results) + 1]] <- result
    copom_calc(parts, x+1, results, conflicts)
  }
}

copom_curve_term <- function(copom_date,
                             zero,
                             copom_forward,
                             move) {
  data.frame(
    copom_date = copom_date,
    zero = zero,
    copom_forward = copom_forward,
    move = move
  )
}

cdi_rate_from_web <- function(refdate = NULL) {
  if (is.null(refdate)) {
    url <- "https://www2.cetip.com.br/ConsultarTaxaDi/ConsultarTaxaDICetip.aspx"
    
    res <- GET(url)
    .json <- content(res, as = "text") |> 
      jsonlite::fromJSON()
    
    refdate <- as.Date(.json$dataTaxa, "%d/%m/%Y")
    divide_by_100 <- \(x) x/100
    
    data.frame(
      refdate = refdate,
      CDI = .json$taxa |>
        str_replace(",", ".") |>
        as.numeric() |>
        divide_by_100() |>
        spotrate("discrete",
                 "business/252",
                 "Brazil/ANBIMA")
    )  
  } else {
    df <- get_series(c(CDI = 4389),
                     start_date = refdate,
                     end_date = refdate)
    data.frame(
      refdate = df$date,
      CDI = spotrate(df$CDI/100,
                     "discrete",
                     "business/252",
                     "Brazil/ANBIMA")
    )
  }
}

plot_curve <- function(curve, copom_dates, base_size = 20) {
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
  names(.colors) <- .names
  
  g <- ggplot() +
    geom_vline(xintercept = copom_dates, colour = .dash,
               linetype = "dashed", size = 1) +
    geom_line(
      data = curve_spt,
      mapping = aes(x = dates, y = rates, colour = .names[1]),
      size = 1
    ) +
    geom_point(
      data = curve_spt,
      mapping = aes(x = dates, y = rates, colour = .names[1]),
      size = 2
    ) +
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
  
  g <- g +
    scale_colour_manual("", breaks = .names, values = .colors)
  
  .title <- glue("Curva de Juros Prefixados DI1 - {refdate}",
                 refdate = format(curve@refdate))
  g <- g +
    labs(x = "Data",
         y = "%",
         title = .title,
         subtitle = "As linhas cinza tracejadas representam as datas do COPOM",
         caption = "Desenvolvido por wilsonfreitas (com dados da B3)") +
    theme_wf(base_size = base_size)
  g
}

plot_copom_curve <- function(curve, copom_curve, copom_dates, base_size = 16) {
  curve_spt <- as.data.frame(curve)
  curve_fwd <- forwardrate(curve)
  curve_fwd <- spotratecurve(
    as.numeric(curve_fwd),
    cumsum(curve_fwd@terms),
    refdate = curve@refdate,
    .copyfrom = curve
  ) |> as.data.frame()
  
  .dash <- "#4f7f81"
  .colors <- c("#e05305", "#fbb407")
  .names <- c("COPOM Forward", "DI1 Forward")
  names(.colors) <- .names
  
  g <- ggplot() +
    geom_vline(
      xintercept = copom_dates,
      colour = "grey",
      linetype = "dashed", size = 1
    ) +
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
    ) +
    geom_step(
      data = copom_curve,
      mapping = aes(x = dates, y = rates_forward, colour = .names[1]),
      size = 1,
      direction = "hv"
    ) +
    geom_point(
      data = copom_curve,
      mapping = aes(x = dates, y = rates_forward, colour = .names[1]),
      size = 2
    )
  
  g <- g +
    scale_colour_manual("", breaks = .names, values = .colors)
  
  .title <- glue("Curva a Termo de Juros Prefixados DI1 - {refdate}",
                 refdate = format(curve@refdate))
  g <- g +
    labs(x = "Data",
         y = "%",
         title = .title,
         subtitle = "As linhas cinza tracejadas representam as datas do COPOM",
         caption = "Desenvolvido por wilsonfreitas (com dados da B3)") +
    theme_wf(base_size = base_size)
  
  g
}

flatten_names <- function(nx) {
  # nx <- txt[c(T,F,F,F,F,F)]
  for (ix in seq_along(nx)) {
    if (nx[ix] != "") {
      last_name <- nx[ix]
    }
    nx[ix] <- last_name
  }
  x <- nx |> str_match("^...")
  as.vector(x)
}

get_contracts <- function(refdate) {
  url <- "http://www2.bmf.com.br/pages/portal/bmfbovespa/lumis/lum-ajustes-do-pregao-ptBR.asp"
  
  if (is.null(refdate)) {
    res <- GET(url)
  } else {
    strdate <- format(as.Date(refdate), "%d/%m/%Y")
    res <- POST(url, body = list(dData1 = strdate), encode = "form")
  }
  
  html <- content(res, as = "text", encoding = "latin1")
  mtx <- str_match(html, "Atualizado em: (\\d{2}/\\d{2}/\\d{4})")
  refdate <- mtx[1,2] |> as.Date("%d/%m/%Y")
  doc <- read_html(html, encoding = "latin1")
  tbl <- xml_find_all(doc, "//table[contains(@id, 'tblDadosAjustes')]")
  
  if (length(tbl) == 0)
    return(NULL)
  
  txt <- tbl[[1]] |>
    xml_find_all("//td") |>
    xml_text() |>
    str_trim() |>
    str_replace("\\.", "") |>
    str_replace(",", ".")
  
  tibble(
    DataRef    = as.Date(refdate),
    Mercadoria = flatten_names(txt[c(T,F,F,F,F,F)]),
    Vencimento = txt[c(F,T,F,F,F,F)],
    PUAnterior = txt[c(F,F,T,F,F,F)] |> as.numeric(),
    PUAtual    = txt[c(F,F,F,T,F,F)] |> as.numeric(),
    Variacao   = txt[c(F,F,F,F,T,F)] |> as.numeric()
  )
}

contract_to_maturity <- function(x) {
  maturity_code <- str_sub(x, -3)
  
  year <- as.integer( str_extract(maturity_code, "\\d\\d$") ) + 2000
  
  m_ <- c(F = 1, G = 2, H = 3, J = 4, K = 5, M = 6, N = 7, Q = 8, U = 9, V = 10, X = 11, Z = 12)
  month_code <- str_extract(maturity_code, "^.")
  month <- m_[month_code] |>
    str_pad(2, pad = "0")
  
  glue("{year}-{month}-01") |> as.Date()
}


get_curve_from_web <- function(refdate = NULL) {
  contracts <- get_contracts(refdate)
  refdate <- contracts$DataRef[1] |> as.Date()
  di1 <- contracts |>
    filter(Mercadoria == "DI1") |>
    mutate(
      maturity_date = contract_to_maturity(Vencimento) |>
        following("Brazil/ANBIMA")
    ) |>
    mutate(
      business_days = bizdays(DataRef, maturity_date, "Brazil/ANBIMA"),
      adjusted_tax = ((100000 / PUAtual)^(252/business_days) - 1)
    ) |>
    rename(refdate = DataRef) |> 
    filter(business_days != 0) |>
    select(maturity_date, refdate, adjusted_tax, business_days)
  
  di1_curve <- spotratecurve(
    di1$adjusted_tax,
    di1$business_days,
    "discrete",
    "business/252",
    "Brazil/ANBIMA",
    refdate = refdate
  )
  
  rates <- cdi_rate_from_web()
  di1_curve[[1]] <- rates$CDI
  
  di1_curve
}

# Paleta de cores
# https://icolorpalette.com/e05305_4f7f81_fbb407_5d91a2_432608
theme_wf <- function(base_size = 12,
                     base_family = "mono",
                     base_line_size = base_size/22,
                     base_rect_size = base_size/22) {
  theme_grey(base_size = base_size,
             base_family = base_family, 
             base_line_size = base_line_size,
             base_rect_size = base_rect_size) %+replace%
    theme(
      legend.position = "top",
      text = element_text(family = base_family, size = base_size),
      plot.background = element_blank(),
      panel.background = element_blank(),
      strip.background = element_blank(),
      panel.grid = element_line(colour = "grey92"),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      title = element_text(family = base_family,
                           colour = "black",
                           face = "bold"),
      axis.line = element_line(colour = "grey92", size = 1),
      axis.title.y = element_text(colour = "black", face = "bold"),
      axis.title.x = element_text(colour = "black", face = "bold"),
      legend.key = element_rect(fill = "white", colour = NA),
      complete = TRUE
    )
}
