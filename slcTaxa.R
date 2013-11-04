
library(XML)

url <- 'http://www2.bmf.com.br/pages/portal/bmfbovespa/boletim1/TxRef1.asp'
doc <- htmlTreeParse(url, useInternalNodes=TRUE)
opt <- xpathSApply(doc, "//option/@value")
opt