
library(httr)
library(dplyr)
library(bizdays)
library(purrr)
library(stringr)
library(fixedincome)

list(
  Type = "ZeroCouponBond",
  Name = "LTN_20110101",
  Currency = "BRL",
  DayCountRule = "business/252",
  Calendar = "Brazil/ANBIMA",
  DiscountCurve = Curve("curve_name"),
  Notional = 1000,
  MaturityDate = as.Date("XXXX-XX-XX"),
  SpotPrice = 998,
  TheoPrice = PricingFunction("function_name")
)

bizdays.options$set(default.calendar = "Brazil/ANBIMA")

url <- modify_url("https://api.morph.io/wilsonfreitas/MorthIO_TitulosPublicos_ANBIMA/data.json",
                  query = list(key = "uIUD3TeyxgIVWhVwsFr0",
                               query = "select * from 'data' where data_referencia in (select max(data_referencia) from 'data') and titulo = 'LTN'"))
res <- GET(url)
df <- jsonlite::fromJSON(content(res, as = "text"))

df <- map_if(df, function(x) any(str_detect(x, "^\\d{8}$")), function(.x) as.Date(.x, format="%Y%m%d")) %>%
  as_tibble() %>%
  select(data_referencia, data_vencimento, taxa_ind, pu) %>%
  rename(yield_rate = taxa_ind, spot_price = pu,
         refdate = data_referencia, maturity_date = data_vencimento) %>%
  mutate(yield_rate = spotrate(yield_rate/100, "discrete", "business/252", "Brazil/ANBIMA"))

df %>% mutate(theo_value = 1000 * discount(yield_rate, refdate, following(maturity_date)))

df$yield_rate
