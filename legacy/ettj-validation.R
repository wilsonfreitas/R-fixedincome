

library(XML)
url <- "http://www2.bmf.com.br/pages/portal/bmfbovespa/boletim1/TxRef1.asp?Data=31/01/2012&Data1=20120131&slcTaxa=PRE"
tbls <- readHTMLTable(url)
tbls$tb_principal1

library(busdays)
data(holidayANDIMA)
source('ts.R')

session.date = as.Date('2013-08-13')
r.df <- read.table('CDIxPRE-2013-08-13.txt', header=TRUE)
head(r.df)
str(r.df)
r.df[,c('r.du', 'r.dc')] = r.df[,c('r.du', 'r.dc')]/100
# dates <- as.Date(session.date + r.df[, 'dc'], origin='1970-01-01')
dates <- as.character(session.date + r.df[, 'dc'])
busdays <- gen.busdays(holidayANDIMA)
du <- apply(as.matrix(dates), 1, function(.) busdays(as.character(session.date), as.character(.)))
# ts.cdi.du <- cbind(du, r.df[,c('r.du')])
# ts.cdi.dc <- cbind(r.df[,c('dc')], r.df[,c('r.du')])

# term structure setup
ts.cdi <- data.frame(Days=dates, Rate=r.df[,c('r.du')], Rate2=r.df[,c('r.dc')])
str(ts.cdi)
ts.cdi.dc <- data.frame(Days=r.df[,c('dc')], Rate=r.df[,c('r.du')])
str(ts.cdi.dc)
ts.cdi.du <- data.frame(Days=du, Rate=r.df[,c('r.du')])
str(ts.cdi.du)

pu.cdi.dc <- apply(ts.cdi, 1, function(.) {
    dc <- as.Date(.[1]) - session.date
    return((1 + as.numeric(.[3]))^(as.numeric(dc)/360))
})

pu.cdi.du <- apply(ts.cdi, 1, function(.) {
    du <- busdays(as.character(session.date), .[1])
    return((1 + as.numeric(.[2]))^(as.numeric(du)/252))
})

plot((pu.cdi.dc - pu.cdi.du)*1e4,
     col='red',
     type='l',
     xlab='',
     ylab='Diferença (bp)',
     main='Diferença entre fatores de composição\nPRExCDI 2013-08-13\nDC-DU')

