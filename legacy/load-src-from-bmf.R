
library(XML)

url <- sprintf('http://www2.bmf.com.br/pages/portal/bmfbovespa/boletim1/TxRef1.asp?Data=%s&Data1=%s&slcTaxa=%s',
    '04/10/2012', '20131004', 'PRE')
doc <- htmlTreeParse(url, useInternalNodes=TRUE)

tit <- xpathSApply(doc, "//td[contains(@class, 'tabelaTitulo')]", xmlValue)

num <- xpathSApply(doc, "//td[contains(@class, 'tabelaConteudo')]", 
    function(x) gsub('[\r\n \t]+', '', xmlValue(x)))

num <- sapply(num, function(x) {
    as.numeric(gsub(',', '.', x))
}, USE.NAMES=FALSE)

idx.t <- seq(1, length(num), 3)
terms <- num[idx.t]
idx.r <- ! (seq(1, length(num)) %in% idx.t)
rates <- num[idx.r]
rates <- rates[seq(2, length(rates), 2)]/100

# cbind(terms, rates)
crv <- as.SpotRateCurve(cbind(terms, rates))
plot(crv, type='l')

