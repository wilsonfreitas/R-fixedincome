
library(busdays)

data(holidayANDIMA)
adj.date <- gen.adj.busday(holidayANDIMA)
busdays <- gen.busdays(holidayANDIMA)

session.date <- '2013-08-13'
aux.df <- read.table('adjDI1.txt', header=TRUE)
months = list(F='01',G='02',H='03',J='04',K='05',M='06',N='07',Q='08',U='09',V='10',X='11',Z='12')
aux.df$MaturityDate <- ''
aux.df$BusinessDays <- 0
aux.df$CurrentDays <- 0

for (k in 1:dim(aux.df)[1]) {
    m.code <- aux.df$Vct[k] # Maturity code
    date <- paste(2000 + as.numeric(substr(m.code,2,3)), months[[substr(m.code,1,1)]], '01', sep='-')
    aux.df$MaturityDate[k] <- adj.date(date)
    aux.df$BusinessDays[k] <- busdays(session.date, aux.df$MaturityDate[k])
    aux.df$CurrentDays[k] <- as.Date(aux.df$MaturityDate[k]) - as.Date(session.date)
}
str(aux.df)
head(aux.df)

ts.fut.du <- aux.df[, c('BusinessDays', 'PU'), drop=FALSE]
names(ts.fut.du) <- c('Days', 'Price')
str(ts.fut.du)
head(ts.fut.du)

ts.cdi.du.fut <- data.frame(Days=ts.fut.du$Days, Rate=(100000/ts.fut.du$Price)^(252/ts.fut.du$Days)-1)
ts.cdi.du.fut <- rbind(ts.cdi.du.fut, c(1, 0.0822))
ts.cdi.du.fut <- ts.cdi.du.fut[order(ts.cdi.du.fut$Days),]
row.names(ts.cdi.du.fut) <- 1:dim(ts.cdi.du.fut)[1]
str(ts.cdi.du.fut)
head(ts.cdi.du.fut)
# r.PU.df <- as.data.frame((100000/PU.df$PU - 1)*(360/PU.df$Days))
#ts.cdi.du.fut$date <- PU.df$Days

idx <- apply(ts.cdi.du.fut, 1, function(.) which(.[1] == ts.cdi.du$Days))
plot((ts.cdi.du[idx,'Rate']-ts.cdi.du.fut$Rate)*1e4,
     type='o',
     col='blue',
     xlab='',
     ylab='Diferença (bp)',
     main='Diferença entre taxas de juros\ndivulgada x futuros\n2013-08-13')


## Preparando dados para interpolação incluindo data de COPOM

ts.cdi.du.1Y <- ts.cdi.du.fut[which(ts.cdi.du.fut$Days <= 252),]

copom.dates.df <- read.table('COPOM_DATES.txt')
copom.dates <- apply(copom.dates.df, 1, function(.) busdays(as.character(session.date), .))
copom.dates <- copom.dates[which(copom.dates > 0 & copom.dates <= 252)]

save(copom.dates, ts.cdi.du.1Y, file='ettj.rda')

## interpolação com data de copom
rm(list=ls())

source('ts.R')
load('ettj.rda')
plot(ts.cdi.du.1Y,
     type='o',
     col='blue',
     xlab='Dias',
     ylab='Taxa (%)',
     main='Taxas de juros a vista\n2013-08-13')
abline(v=copom.dates, col='grey')

rates <- flat.forward.copom.rates(ts.cdi.du.1Y, copom.dates[c(1,2,3)])
plot(ts.cdi.du.1Y,
     type='o',
     col='blue',
     xlab='Dias',
     ylab='Taxa (%)',
     main='Taxas de juros a vista considerando\nReuniões do COPOM\n2013-08-13')
abline(v=copom.dates[1:3], col='grey')
lines(rates,
      type='o',
      col='red')

