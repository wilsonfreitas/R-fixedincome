
library(stringr)
library(fixedincome)
library(bizdays)
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

calANBIMA <- Calendar(holidaysANBIMA, weekdays=c('saturday', 'sunday'), dib=252, name='ANBIMA')
calACTUAL <- Calendar(name='Actual')

tickers_map <- list(
	SLP=list(
		columns=1,
		compounding='discrete',
		daycount='business/252',
		calendar=calANBIMA,
		interp=flatforward),
	PRE=list(
		columns=2,
		compounding='discrete',
		daycount='business/252',
		calendar=calANBIMA,
		interp=flatforward),
	DOL=list( # cupom sujo
		columns=1,
		compounding='simple',
		daycount='actual/360',
		calendar=calANBIMA,
		interp=flatforward),
	DIM=list( # cupom de IGPM
		columns=1,
		compounding='discrete',
		daycount='business/252',
		calendar=calANBIMA,
		interp=flatforward),
	DIC=list( # cupom de IPCA
		columns=1,
		compounding='discrete',
		daycount='business/252',
		calendar=calANBIMA,
		interp=flatforward)
)

#' selic_curve <- getCurve('SELIC', '2013-01-10')
getCurve <- function(ticker, date) {
	date <- as.Date(date)
	ticker_info <- tickers_map[[ticker]]
	url <- getURL(ticker, date)
	num <- downloadData(url)
	if (ticker_info$columns == 1) {
		terms <- num[c(TRUE, FALSE)]
		value <- num[c(FALSE, TRUE)]/100
	} else if (ticker_info$columns == 2) {
		terms <- num[c(TRUE, FALSE, FALSE)]
		value <- num[c(FALSE, FALSE, TRUE)]/100
	}
	rates <- as.spotrate(value, as.compounding(ticker_info$compounding),
		as.daycount(ticker_info$daycount), ticker_info$calendar)
	as.spotratecurve(terms+date, rates, refdate=date, interp=ticker_info$interp, name=ticker)
}
print(selic)

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
require(ggplot2)
dt <- as.Date('2013-10-18')
selic <- getCurve('PRE', '2014-07-10')
selic.l <- head(selic, 20)
.terms <- seq(1, max(terms(selic.l, as.x=TRUE)))
s.selic.l <- subcurve(selic.l, .terms)
interp(selic.l) <- natural.spline
s.selic.li <- subcurve(selic.l, .terms)
ggplot() +
	geom_line(data=as.data.frame(s.selic.l), aes(x=terms, y=rates), colour='red')+
	geom_line(data=as.data.frame(s.selic.li), aes(x=terms, y=rates), colour='magenta')+
	geom_point(data=as.data.frame(selic.l), aes(x=terms, y=rates), colour='black', shape=16)
#     geom_line(data=as.data.frame(h_selic[1:91]), aes(x=terms, y=rates), colour='green') +
#     geom_line(data=as.data.frame(m_selic[1:91]), aes(x=terms, y=rates), colour='blue') +
#     theme_bw() 
