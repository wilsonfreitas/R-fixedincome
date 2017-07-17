
library(transmute)
library(httr)
library(dplyr)
library(bizdays)

bizdays.options$set(default.calendar = "Brazil/ANBIMA")



tr_ <- transmuter(
  match_regex("^\\d{8}$", function(text, match) as.Date(text, format="%Y%m%d"))
)

url <- modify_url("https://api.morph.io/wilsonfreitas/MorthIO_TitulosPublicos_ANBIMA/data.json",
                  query = list(key = "uIUD3TeyxgIVWhVwsFr0",
                               query = "select * from 'data' where data_referencia in (select max(data_referencia) from 'data') and titulo = 'LTN'"))
res <- GET(url)

df <- jsonlite::fromJSON(content(res, as = "text"))

match_regex <- function(regex, any_all = any) {
  function(x) {
    if (! is.character(x)) return(FALSE)
    r <- grepl(regex, x, perl = TRUE)
    any_all(r)
  }
}

regmatches(df$data_vencimento, regexpr("^(?<year>\\d{4})(?<month>\\d{2})(?<day>\\d{2})", df$data_vencimento, perl =TRUE))

str(regexpr("^(?<year>\\d{4})(?<month>\\d{2})(?<day>\\d{2})", df$data_vencimento, perl =TRUE))

stringr::str_match(df$data_vencimento, "^(?<year>\\d{4})(?<month>\\d{2})(?<day>\\d{2})")

regmatches(df$data_referencia, regexec("^(?<year>\\d{4})(?<month>\\d{2})(?<day>\\d{2})", df$data_vencimento, perl =TRUE))

df %>% purrr::dmap_if(., match_regex("^\\d{8}$", any_all = all), function(text) as.Date(text, format="%Y%m%d"))

df %>% mutate_if(., function(x) {
  r <- grepl("^\\d{8}$", x, perl = TRUE)
  any(r) & is.character(x)
}, function(text) as.Date(text, format="%Y%m%d"))

# transmute::transmute(tr_, df)

df <- within(df, {
  yield_rate = spotrate(taxa_ind/100, "discrete", "business/252", "Brazil/ANBIMA")
  theo_pu = 1000 * discount(yield_rate, data_referencia, following(data_vencimento))
})

str(df)

df$yield_rate
