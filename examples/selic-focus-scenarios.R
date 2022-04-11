
library(rbcb)
library(purrr)
library(stringr)
library(ggplot2)
library(dplyr)

selic <- get_series(c(SELIC = 432), start_date = "2022-04-01")

selic_exp <- get_market_expectations("selic", start_date = "2021-03-25") |>
  filter(baseCalculo == 0) |>
  mutate(Reuniao = str_split(Reuniao, "/") |>
    map_chr(\(x) paste0(x[2], x[1]))) |>
  arrange(Reuniao)

selic_exp |>
  filter(Reuniao >= "2022R1") |>
  ggplot(aes(x = Reuniao, y = Mediana, group = Data, color = Data)) +
  geom_point() +
  geom_line()
