
library(stringr)
library(fixedincome)
library(XML)

# supplant('{media}logo.png', list(media='http://aboutwilson.net/'))
# supplant("{media}/logo.gif, {media}/img/ {face}",
# 	list(media='http://aboutwilson.net', face='wilson'))

str_supplant <- function (string, repl) {
	result <- str_match_all(string, "\\{([^{}]*)\\}")
	if (length(result[[1]]) == 0)
		return(string)
	result <- result[[1]]
	for (i in seq_len(dim(result)[1])) {
		x <- result[i,]
		pattern <- x[1]
		key <- x[2]
		if (!is.null(repl[[key]]))
			string <- gsub(pattern, repl[[key]], string, perl=TRUE)
	}
	string
}

# "DIC" DIxIPCA compounded 1
# "DIM" DIxIGP-M compounded 1
# "DOL" DIxUSD simple 1
# "BRP" IBrX-50 ind 1
# "PRE" DIxPRE compounded 2

# "ACC" Ajuste cupom simple 1
# "ALD" Aluminio US$/t 1
# "AN" DIxAnbid compounded 2
# "ANP" AnbidxPRE compounded 2
# "APR" Ajuste PRE compounded 2
# "CBD" Cobre US$/t 1
# "DOC" Cupom limpo simple 1
# "DP" USDxPRE compounded 2
# "EUC" DIxEUR simple 1 
# "EUR" EUR price 1
# "IAP" IPCA ind 1
# "INP" Ibovespa ind 1
# "IPR" IGP-M ind 1
# "JPY" JPY price 1
# "LIB" LIBOR simple 1
# "NID" Niquel US$/t 1
# "PBD" Chumbo US$/t 1
# "PDN" Prob. no-default 1
# "PTX" USD price 1
# "SDE" Spread Libor EURxUSD additive 1
# "SLP" SELIC compounded 1
# "SND" Estanho US$/t 1
# "TFP" TBFxPRE compounded 2
# "TP" TRxPRE compounded 2
# "TR" DIxTR compounded 2
# "ZND" Zinco US$/t 1
# "TODOS"

getURL <- function(ticker, date) {
	url <- 'http://www2.bmf.com.br/pages/portal/bmfbovespa/boletim1/TxRef1.asp'
	query <- str_supplant('?Data={date}&Data1={sysdate}&slcTaxa={ticker}',
		list(date=format(as.Date(date), '%d/%m/%Y'),
			sysdate=format(Sys.Date(), '%Y%m%d'),
			ticker=ticker))
	paste0(url, query)
}

business_days_between <- function(from, to) bizdays(from, to)
actual_days_between <- function(from, to) to - from

tickers_map <- list(
	SELIC=list(
		ticker='SLP',
		columns=1,
		compounding='compounded',
		dib=252,
		weight=0.01,
		days_between=business_days_between),
	DI1=list(
		ticker='PRE',
		columns=2,
		compounding='compounded',
		dib=252,
		weight=0.01,
		days_between=business_days_between),
	CC=list(
		ticker='DOL',
		columns=1,
		compounding='simple',
		dib=360,
		weight=0.01,
		days_between=business_days_between),
	IGPM=list(
		ticker='DIM',
		columns=1,
		compounding='compounded',
		dib=252,
		weight=0.01,
		days_between=business_days_between),
	IPCA=list(
		ticker='DIC',
		columns=1,
		compounding='compounded',
		dib=252,
		weight=0.01,
		days_between=business_days_between),
	IBRX=list(
		ticker='BRP',
		columns=1,
		compounding='simple',
		dib=252,
		weight=0.01,
		days_between=business_days_between)
)

#' selic_curve <- getCurve('SELIC', '2013-01-10')
getCurve <- function(ticker, date) {
	bmf_ticker <- tickers_map[[ticker]]
	url <- getURL(bmf_ticker$ticker, date)
	num <- downloadData(url)
	if (bmf_ticker$columns == 1) {
		terms <- num[c(TRUE, FALSE)]
		value <- num[c(FALSE, TRUE)]
	} else if (bmf_ticker$columns == 2) {
		terms <- num[c(TRUE, FALSE, FALSE)]
		value <- num[c(FALSE, FALSE, TRUE)]
	}
	value <- value*bmf_ticker$weight
	terms <- bmf_ticker$days_between(as.Date(date), as.Date(date) + terms)
	SpotRateCurve(value, terms,
		dib=bmf_ticker$dib, compounding=bmf_ticker$compounding,
        name=bmf_ticker$ticker, datum=date)
}

downloadData <- function (url) {
	doc <- htmlTreeParse(url, useInternalNodes=TRUE)
	tit <- xpathSApply(doc, "//td[contains(@class, 'tabelaTitulo')]", xmlValue)
	num <- xpathSApply(doc, "//td[contains(@class, 'tabelaConteudo')]", 
	    function(x) gsub('[\r\n \t]+', '', xmlValue(x)))
	sapply(num, function(x) {
		as.numeric(gsub(',', '.', x))
	}, USE.NAMES=FALSE)
}

# code examples
# require(ggplot2)
# selic <- getCurve('SELIC', '2013-10-18')
# ff_selic <- CurveInterpolation(selic)
# h_selic <- CurveInterpolation(selic, hermite)
# m_selic <- CurveInterpolation(selic, monotone)
# ggplot(as.data.frame(head(selic, 20)), aes(x=terms, y=rates)) +
#     geom_line(data=as.data.frame(ff_selic[1:91]), aes(x=terms, y=rates), colour='red') +
#     geom_line(data=as.data.frame(h_selic[1:91]), aes(x=terms, y=rates), colour='green') +
#     geom_line(data=as.data.frame(m_selic[1:91]), aes(x=terms, y=rates), colour='blue') +
#     geom_point(colour='black', shape=16) +
#     theme_bw() 
