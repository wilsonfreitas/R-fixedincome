
# Pricing LFT Instruments

rm(list=ls(all.names=TRUE))

library(Quandl)
library(fixedincome)
library(bizdays)

truncate <- function(x, n=0) trunc(x*10^n)/(10^n)

refdate <- as.Date('2014-03-21')
cal <- Calendar(holidays=holidaysANBIMA, name='ANBIMA', weekdays=c('saturday', 'sunday'))
dc <- as.daycount('business/252')
disc <- as.compounding('discrete')

selic <- Quandl('BCB/1178', 'raw', '2000-07-01', as.character(refdate), sort='asc')
selic <- within(selic, {
	Date <- as.Date(Date)
	Rate <- as.spotrate(Value/100, disc, dc, cal)
	Factor <- round(compound(Rate, '1 days'), 8)
	CumFactor <- cumprod(Factor)
	VNA <- truncate(1000*CumFactor, 6)
	VNA <- c(NA, VNA[1:(length(VNA)-1)])
})

head(selic)
tail(selic)

# Pricing LFT Instruments
tit_pub <- read.table(format(refdate, 'ms%y%m%d.txt'), skip=2, sep='@', header=TRUE)
lft_quot <- subset(tit_pub, Titulo == 'LFT',
	select=c('Data.Referencia', 'Data.Vencimento', 'Tx..Indicativas', 'PU'))
names(lft_quot) <- c('RefDate', 'Maturity', 'Spread', 'SpotPrice')

VNA <- with(selic, VNA[Date == refdate])
lft_quot <- within(lft_quot, {
	Spread <- as.numeric(sub(',', '.', Spread))
	Spread <- as.spotrate(Spread/100, disc, dc, cal)
	SpotPrice <- as.numeric(sub(',', '.', SpotPrice))
	RefDate <- as.Date(as.character(RefDate), format='%Y%m%d')
	Maturity <- as.Date(as.character(Maturity), format='%Y%m%d')
	Quotation <- truncate(100*discount(Spread, from=RefDate, to=adjust.next(Maturity, cal)), 4)
	TheoPrice <- truncate(VNA*Quotation/100, 6)
})

lft_quot

with(lft_quot, SpotPrice-TheoPrice)

