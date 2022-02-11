
Calendar <- function(holidays) {
	dates <- seq(from=min(holidays), to=max(holidays), by='day')
	is.workday <- vapply(dates, function(.) {
		w <- as.POSIXlt(.)$wday
		return(! (w == 6 || w == 0 || any(. == holidays)))
	}, 0)
	idx <- 0
	index <- vapply(is.workday, function(.) {
		idx <<- idx + ifelse(., 1, 0)
		return(idx)
	}, 0)
	cal <- data.frame(dates, is.workday, index)
	list(
		workdays_between=function(from, to) {
			from.is.workday <- cal$is.workday[which(dates == from)]
			from.idx <- cal$index[which(dates == from)]
			to.idx <- cal$index[which(dates == to)]
			return(to.idx - from.idx - ifelse(from.is.workday, 0, 1))
		},
		adjust_to_next_workday=function(date) {
			while (! cal$is.workday[which(dates == date)])
				date = date + 1
			return(date)
		},
		adjust_to_previous_workday=function(date) {
			while (! cal$is.workday[which(dates == date)])
				date = date - 1
			return(date)
		}
	)
}

InterestRate <- function(rate, frequency, compounding, daycount, calendar) {
	list(
		rate=rate,
		frequency=frequency,
		compounding=compounding,
		daycount=daycount,
		calendar=calendar
	)
}

CompoundingFactor <- function(f, t) list(f=f, t=t)

test.InterestRate <- function() {
	ir <- InterestRate(0.09, 'annual', 'compounded', 'business/252')
	ir$compound(Period(from, to))
}

test.CompoundingFactor <- function() {
	cf <- CompoundingFactor(1.11, Period(from, to))
}

implied.rate <- function(f, t=f$t, dib=360) rate(f$f^(dib/t)-1, t)

compound <- function(r, t=r$t, dib=360) comp.factor((1+r$r)^(t/dib), t)

compound.div <- function(d, u) comp.factor(u$f/d$f, u$t-d$t)

compound.mul <- function(d, u) comp.factor(u$f*d$f, u$t+d$t)

discount <- function(r, t=r$t, dib=360) comp.factor(1/compound(r, t, dib), t)

fwd.rate <- function(d, u, dib=360) implied.rate(compound.div(compound(d, dib=dib),compound(u, dib=dib)))

flat.forward.rates <- function(ts, t, dib=360) {
    if (any(ts$Days == t)) {
        idx <- (ts$Days == t)
        return(rate(ts[idx,2],t))
    }
    idx.u <- min(which(ts$Days > t))
    idx.d <- max(which(ts$Days < t))
    u <- rate(ts$Rate[idx.u], ts$Days[idx.u])
    d <- rate(ts$Rate[idx.d], ts$Days[idx.d])
    f <- fwd.rate(d, u)
    implied.rate(compound.mul(compound(d, dib=dib), compound(f,t-d$t, dib=dib)), t, dib=dib)
}


forward.rates <- function(ts, t, dib=360) { # daily forward rates
    if (t == 1) return( rate(ts$Rate[1], t) )
    r.i <- flat.forward.rates(ts, t, dib=dib)
    r.p <- flat.forward.rates(ts, t-1, dib=dib)
    fwd.rate(r.p, r.i, dib=dib)
}

flat.forward.copom.rates <- function(ts, copom.dates) {
    # TODO: forbid 22 patterns
    #
    days.table <- cbind(sort(union(copom.dates, ts$Days)), NA)
    # Column 2 represents
    #   1:discount curve points
    #   2:copom points
    days.table[,2] <- apply(days.table, 1, function (.) if (any(.[1] == copom.dates) ) 2 else 1)
    
    rates <- matrix(NA, length(days.table[,1]), 3)
    rates[,1] <- days.table[,1]
    colnames(rates) <- c('Days', 'Rate', 'FwdRate')
    
    for ( i in days.table[,1] ) {
        .i <- which(i == days.table[,1])
        if ( i == 1 ) {
            rates[.i,'Rate'] = ts$Rate[1]
            rates[.i,'FwdRate'] = ts$Rate[1]
        } else {
            .s <- paste(days.table[c(.i-1,.i),2], collapse='')
            if (.s == '12') {
                rates[.i,'FwdRate'] = rates[.i-1,'FwdRate']
                f.prev <- compound(rate(rates[.i-1,'Rate'], rates[.i-1,'Days']))
                f.curr <- compound(rate(rates[.i,'FwdRate'], i-rates[.i-1,'Days']))
                rates[.i,'Rate'] = implied.rate(compound.mul(f.prev, f.curr), i)$r
            } else if (.s == '21' || .s == '11') {
                rates[.i,'Rate'] = ts$Rate[which(i == ts$Days)]
                r.1 <- rate(rates[.i-1, 'Rate'], rates[.i-1, 'Days'])
                r.2 <- rate(rates[.i,'Rate'], rates[.i,'Days'])
                rates[.i,'FwdRate'] = fwd.rate(r.1, r.2)$r
            }
        }
    }
    return(as.data.frame(rates[,c('Days', 'Rate')]))
}

copom.flat.forward.rates.2 <- function(ts, copom.dates) {
    days.table <- cbind(sort(union(copom.dates, ts$Days)), NA)
    # Column 2 represents
    #   1:discount curve points
    #   2:copom points
    days.table[,2] <- apply(days.table, 1, function (.) if (any(.[1] == copom.dates) ) 2 else 1)
    
    rates <- matrix(NA, max(days.table[,1]), 4)
    rates[,1] <- 1:max(days.table[,1])
    colnames(rates) <- c('Days', 'Rate', 'FwdRate', 'CumRate')
    
    idx <- min(days.table[,1]):max(days.table[,1])
    for ( i in idx ) {
        if ( i == 1 ) {
            .i <- which(i == ts$Days)
            rates[i,'Rate'] = ts$Rate[.i]
            rates[i,'FwdRate'] = ts$Rate[1]
            rates[i,'CumRate'] = (1 + ts$Rate[1])^(1/360)
        } else {
            .i <- min(which(i <= days.table[,1]))
            .s <- paste(days.table[c(.i-1,.i),2], collapse='')
            if (.s == '12') {
                rates[i,'FwdRate'] = rates[i-1,'FwdRate']
                rates[i,'CumRate'] = rates[i-1,'CumRate']*(1 + rates[i,'FwdRate'])^(1/360)
                rates[i,'Rate'] = rates[i,'CumRate']^(360/i) - 1
            } else if (.s == '21' || .s == '11') {
                .d <- which(days.table[.i-1,1] == rates[,'Days'])
                d <- rate(rates[.d,'Rate'], rates[.d,'Days'])
                f <- fwd.rate(d, rate(ts$Rate[which(days.table[.i,1] == ts$Days)], days.table[.i,1]))
                
                rates[i,'Rate'] = implied.rate(compound.mul(compound(d), compound(f,i-d$t)), i)$r
                rates[i,'FwdRate'] = fwd.rate(rate(rates[i-1, 'Rate'], i-1), rate(rates[i,'Rate'], i))$r
                rates[i,'CumRate'] = rates[i-1,'CumRate']*(1 + rates[i,'FwdRate'])^(1/360)
            }
        }
    }
    return(rates)
}

