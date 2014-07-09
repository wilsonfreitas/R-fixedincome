
library(bizdays)
data(holidaysANBIMA)
cal <- Calendar(holidaysANBIMA)
selic <- read.table('SELIC.txt', header=F, skip=2, sep=';')
head(selic)
selic[,1] <- as.Date(selic[,1], format='%d/%m/%Y')
to.double <- function(x) sub(',', '.', sub('\\.', '', x))
selic[,2] <- as.numeric(to.double(selic[,2]))
bizdays.of.2012 <- bizseq(cal, '2010-01-01', '2012-12-31')
bcbdays.of.2012 <- selic[selic[,1] >= '2010-01-01' & selic[,1] <= '2012-12-31', 1]
all.equal(bizdays.of.2012, bcbdays.of.2012)
# library(httr)
# b <- new_bin()
# POST(b)
# POST(b, body = "A simple text string")
# POST(b, body = list(a = 1, b = 2, c = 3), verbose())
# 
# p <- POST('http://www3.bcb.gov.br/selic/consulta/taxaSelic.do',
# 	body=list(tipoApresentacao='arquivo', fatorCorrecao='asc',
# 		method='listarTaxaDiaria', idioma='P',
# 		dataInicial='25/10/2013', dataFinal='25/10/2013'), verbose())
# print(content(p, as='text'))
# 
# IDI <- bmfh.get.data(ticker='IDI')
# CDI <- bmfh.get.data(ticker='CDI')
# DI1 <- bmfh.get.data(ticker='DI1')
# SELIC <- bmfh.get.data(ticker='SELIC')
# VID <- bmfh.get.data(ticker='VID')
# VIDO <- bmfh.get.data(ticker='VIDO')
# CDI$f_vl_referencia[CDI$c_dt_referencia == '2013-07-12']

getISESpot <- function (date) {
    SELIC <- xts(SELIC[,'f_vl_referencia']/100, order.by=SELIC[,'c_dt_referencia'])
    SELIC.compounding <- (1 + SELIC)^(1/252)
    SELIC <- merge(SELIC, SELIC.compounding)
    date_range <- paste('2013-05-27/', date, sep='')
    # ISE_[2013-05-27] = 258923.51
    ISE_0 <- 258923.51 * prod(SELIC[date_range, 'SELIC.compounding'])
    ISE_0
}

getIDISpot <- function (date) {
    as.numeric(subset(IDI, c_dt_referencia == date, select=c('f_vl_referencia')))
}

getSELICSpot <- function (date) {
    SELIC <- xts(SELIC[,'f_vl_referencia']/100, order.by=SELIC[,'c_dt_referencia'])
    SELIC[date]
}

getSELICZeroCurve <- function (date) {
    url <- sprintf('http://www2.bmf.com.br/pages/portal/bmfbovespa/boletim1/TxRef1.asp?Data=%s&Data1=%s&slcTaxa=%s',
        format(as.Date(date), '%d/%m/%Y'), '20131021', 'SLP')
    doc <- htmlTreeParse(url, useInternalNodes=TRUE)
    tit <- xpathSApply(doc, "//td[contains(@class, 'tabelaTitulo')]", xmlValue)
    num <- xpathSApply(doc, "//td[contains(@class, 'tabelaConteudo')]", 
        function(x) gsub('[\r\n \t]+', '', xmlValue(x)))
    
    num <- sapply(num, function(x) {
        as.numeric(gsub(',', '.', x))
    }, USE.NAMES=FALSE)
    
    idx.t <- seq(1, length(num), 2)
    terms <- num[idx.t]
    idx.r <- ! (seq(1, length(num)) %in% idx.t)
    rates <- num[idx.r]/100
    terms <- bizdays(calANBIMA, date, as.Date(date) + terms)
    CurveInterpolation(SpotRateCurve(rates, terms,
        dib=252, compounding='compounded'))
}

getVIDATMCurve <- function (date) {
    atm <- subset(VID, dt_ref == date & f_delta == 50)
    count.terms <- dim(atm)[1]
    atm <- subset(transform(atm,
        biz_days=bizdays(calANBIMA, date, d_vencimento)),
        select=c(f_taxa_m, biz_days))
    vols <- atm$f_taxa_m^2 * atm$biz_days/252
    curve <- SpotRateCurve(vols, atm$biz_days, dib=252)
    curve <- CurveInterpolation(curve, method='linear')
    function (n) {
        sqrt(curve[n]/(n/252))
    }
}

getCOPOMDates <- function(date, n=3) {
    copom.dates <- read.csv('data/COPOM_DATES.txt')
    copom.dates <- as.Date(copom.dates[,1])
    copom.dates <- copom.dates[copom.dates > date][1:n]
    copom.dates <- bizdays(calANBIMA, session.date, copom.dates)
    copom.dates
}

generate.flatforward.copom <- function (session.date) {
    days.table <- stack(list(c=getCOPOMDates(session.date), d=terms(curve)))
    idx <- order(days.table[1])
    days.table <- days.table[idx,]
    # Column 2 represents
    #   d:discount curve points
    #   c:copom points
    
    list(
        interp=function(curve, term, interp.FUN) {
            log.price <- interp.FUN(term)
            price <- exp(log.price)
            price^(dib(curve)/term) - 1
        },
        prepare=function(curve) {
            if ((re <- regexpr('^ddc', paste(days.table[,2], collapse=''))) > 0) {
                days.table <- days.table[-(re+1),]
                curve <- curve[-terms(curve)[re+1]]
            }
            if ((re <- regexpr('cddc', paste(days.table[,2], collapse=''))) > 0) {
                curve <- curve[-days.table[(re+1),1]]
                days.table <- days.table[-(re+1),]
            }
            rates <- matrix(NA, length(days.table[,1]), 3)
            rates[,1] <- days.table[,1]
            colnames(rates) <- c('Days', 'Rate', 'FwdRate')
            for ( i in days.table[,1] ) {
                .i <- which(i == days.table[,1])
                if ( i == 1 ) {
                    rates[.i,'Rate'] = curve[1]
                    rates[.i,'FwdRate'] = curve[1]
                } else {
                    .s <- paste(days.table[c(.i-1,.i),2], collapse='')
                    if (.s == 'dc') {
                        rates[.i,'FwdRate'] = rates[.i-1,'FwdRate']
                        sr.prev <- SpotRate(rates[.i-1,'Rate'], rates[.i-1,'Days'])
                        sr.curr <- SpotRate(rates[.i,'FwdRate'], i-rates[.i-1,'Days'])
                        f.prev <- as.CompoundFactor(sr.prev)
                        f.curr <- as.CompoundFactor(sr.curr)
                        comp <- compound.factor(f.prev*f.curr)
                        rates[.i,'Rate'] = rate(CompoundFactor(comp, i))
                    } else if (.s == 'cd' || .s == 'dd') {
                        rates[.i,'Rate'] = curve[terms(curve)[which(i == terms(curve))]]
                        r.1 <- SpotRate(rates[.i-1, 'Rate'], rates[.i-1, 'Days'])
                        r.2 <- SpotRate(rates[.i,'Rate'], rates[.i,'Days'])
                        rates[.i,'FwdRate'] = rate(forward.rate(r.1, r.2))
                    }
                }
            }
            prices <- (1 + rates[,'Rate'])^(rates[,'Days']/dib(curve))
            interp.coords <- xy.coords(rates[,'Days'], log(prices))
            approxfun(interp.coords, method='linear')
        }
    )
}




