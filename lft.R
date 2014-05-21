
# LFT

options(digits=10)

truncate <- function(x, n=0) {
  k <- 10^n
  trunc(x*k)/k
}

# truncate(1.236, 2)
# round(1.236, 2)

## Computing VNA

tr_selic <- function(ds) {
    ds <- transform(ds, Date=as.Date(Date), Factor=round((1 + Value/100)^(1/252), 8))
    ds <- transform(ds, CumFactor=cumprod(Factor))
    transform(ds, VNA=truncate(1000*CumFactor, 6))
}

# 21/03/2014
# VNA    Taxa SELIC (D-1)
# 210100    6.023,149269    10,65

# selic_bcb <- read.csv('SELIC.csv')
# selic_bcb <- selic_bcb[,c('Data', 'Taxa')]
# selic_bcb$Data <- as.Date(selic_bcb$Data, format='%d/%m/%Y')
# names(selic_bcb) <- c('Date', 'Value')
# selic_bcb <- subset(selic_bcb, Date >= '2000-07-01' & Date <= '2014-03-21')
# selic_bcb <- tr_selic(selic_bcb)
# str(selic_bcb)
# head(selic_bcb)
# tail(selic_bcb)
# 
# selic_bcb$Value - selic$Value

library(Quandl)

library(bizdays)
holidaysANBIMA <- as.Date(unlist(read.table('ANBIMA.cal')))
cal <- Calendar(holidaysANBIMA)
bizdays.options$set(default.calendar=cal)

session_date <- as.Date('2014-03-21')
token <- 'nJ1NhTYdEs2p3MsS4CVd'
selic <- Quandl('BCB/1178', 'raw', '2000-07-01', as.character(session_date), 
    authcode=token, sort='asc')
# selic <- selic[order(selic$Date),]
# selic <- subset(selic, Date >= '2000-07-01' & Date <= session_date)
# selic <- selic[is.bizday(cal, selic$Date),]
selic <- tr_selic(selic)

str(selic)
head(selic)
tail(selic)

## Computing Theoretical Value

tr_lft <- function (ds) {
    transform(ds, Spread=as.numeric(gsub(",", ".", Spread)),
        SpotPrice=as.numeric(gsub(",", ".", gsub("\\.", "", SpotPrice))))
}
sqliter::execute(DBM, 'derivativos', "select data_ref, VNA from titulos where cod_titulo = 'LFT' group by data_ref")

lft_quotes <- sqliter::execute(DBM, 'derivativos', "select data_vcto as Maturity, taxa as Spread, pu as SpotPrice from titulos where cod_titulo = 'LFT' and data_ref = :data_ref", data_ref='2014-03-21', 
)

tit_pub <- read.table('ms140321.txt', skip=2, sep='@', header=TRUE)
lft_quotes <- subset(tit_pub, Titulo == 'LFT')
lft_quotes <- lft_quotes[, c('Data.Referencia', 'Data.Vencimento', 'Tx..Indicativas', 'PU')]
names(lft_quotes) <- c('RefDate', 'Maturity', 'Spread', 'SpotPrice')

lft_quotes <- transform(lft_quotes,
    Spread=as.numeric(sub(',', '.', Spread)),
    SpotPrice=as.numeric(sub(',', '.', SpotPrice)),
    Maturity=as.Date(as.character(Maturity), format='%Y%m%d'))

lft_quotes <- transform(lft_quotes,
    MaturityAdj=adjust.next(cal, Maturity))

lft_quotes <- transform(lft_quotes,
    BusinessDay=bizdays(cal, session_date, MaturityAdj))

cal <- Calendar(holidaysANBIMA, dib=252)
curve <- irc(lft_quotes$MaturityAdj, lft_quotes$Spread/100, calendar=cal,
	compound='disc', refdate=lft_quotes$RefDate[1])

lft_quotes <- transform(lft_quotes,
    Quotation=truncate(100/compfactor(curve, MaturityAdj), 4))

VNA <- unlist(subset(selic, Date == bizdays::offset(cal, session_date, -1),
    select=c('VNA')))
lft_quotes <- transform(lft_quotes,
    TheoPrice=truncate(VNA*Quotation/100, 6))

lft_quotes <- transform(lft_quotes, Diff=SpotPrice-TheoPrice)

all(lft_quotes$Diff == 0)

str(lft_quotes)