
library(testthat)
test_file('test-calendar.R')
source('calendar.R')
cal <- calendar(holidays)
iter <- cal$seqi(as.Date('2013-01-01'), as.Date('2013-01-05'))
iter$get.next()
length(cal$seq('2013-09-01', '2013-09-30'))
cal$days.between('2013-09-01', '2013-09-30')

holidays <- as.Date(unlist(read.table('ANBIMA.cal')))
n.holidays <- as.numeric(holidays)
dates <- seq(from=min(holidays), to=max(holidays), by='day')
n.dates <- as.numeric(dates)

length(dates)
class(dates)

.workday <- lapply(seq_along(dates), function(.) .)
.workday <- lapply(dates, function(.) .)
.workday <- lapply(seq_along(dates), function(.) {
    return( any(dates[.] == holidays) )
})

.workday <- lapply(dates, function(.) {
    wday <- as.POSIXlt(.)$wday
    return( ! (wday == 0 || wday == 6 || any(date == holidays)) )
})
.workday <- vapply(n.dates, function(.) {
    wday <- .%%7
    return( ! ( wday == 2 || wday == 3 || any(. == n.holidays)) )
}, logical(1))

length(.workday)
class(.workday)
head(.workday)
head(unlist(.workday))
is.workday <- unlist(lapply(n.dates, function(.) {
    wday <- .%%7
    return( ! ( wday == 2 || wday == 3 || any(. == n.holidays)) )
}))
length(is.workday)
class(is.workday)
head(is.workday)
head(unlist(is.workday))
which(unlist(.workday) != unlist(is.workday))
### Flat-forward interpolation taking COPOM's meeting into account

```{r}

idx <- seq(min(r.sample$Days), max(r.sample$Days))
r.sample.ffw.copom <- cbind(as.data.frame(idx), apply(as.matrix(idx), 1, function(x) { copom.flat.forward.rates(r.sample, x, d1) }))
names(r.sample.ffw.copom) <- c('Days', 'Rate')
head(r.sample.ffw.copom)
r.sample.fwd.copom <- cbind(as.data.frame(idx), apply(as.matrix(idx), 1, function(x) { forward.rates(r.sample.ffw.copom, x) }))
names(r.sample.fwd.copom) <- c('Days', 'Rate')
```



```{r}
plot(r.sample.fwd.copom, type='n')

abline(v=d1, col="grey")
abline(v=d2, col="grey")
abline(v=d3, col="grey")

lines(r.sample.ffw.copom, col='coral3', lwd=2)
points(r.sample.fwd.copom, col='coral3', pch='.', cex=3)

lines(r.sample.ffw, col='darkolivegreen4', lwd=2)
points(r.sample.fwd, col='darkolivegreen4', pch='.', cex=3)

points(r.sample, col='blue4', pch=18, cex=2)
#lines(r.sample.fwd, col='antiquewhite3')
```


copom.flat.forward.rates <- function(ts, t, copom.date) {
  if (all(copom.date > ts$Days)) {
    stop('invalid copom.date!')
  }
  
  if (any(ts$Days == t)) {
    idx <- (ts$Days == t)
    return(ts[idx,2])
  }
  
  if ( all(t < copom.date) ) { # any( t > copom.date ) == FALSE
    return( ts[1,2] )
  } else { # if (t >= copom.date[1] && t < ts$Days[2]) {
    t.u <- ts$Days[2]
    t.d <- as.numeric(copom.date[1] - 1)
    r.u <- ts$Rate[2]
    r.d <- ts$Rate[1]
    #   } else {
    #     idx.u <- min(which(ts$Days > t))
    #     idx.d <- max(which(ts$Days < t))
    #     r.u <- ts$Rate[idx.u]
    #     r.d <- ts$Rate[idx.d]
    #     t.u <- ts$Days[idx.u]
    #     t.d <- ts$Days[idx.d]
  }
  
  r.fwd <- (((1 + r.u)^(t.u/360))/((1 + r.d)^(t.d/360)))^(360/(t.u - t.d)) - 1
  (((1 + r.d)^(t.d/360))*((1 + r.fwd)^((t - t.d)/360)))^(360/t) - 1
}





# http://www4.bcb.gov.br/pec/taxas/batch/taxas.asp?id=txdolar

library(httr)
GET("http://www4.bcb.gov.br/", path="pec/taxas/batch/taxas.asp", query=c(id='txdolar'),
    use_proxy('10.0.0.20', 80, 'wfreitas', 'A1!2qqww'))


