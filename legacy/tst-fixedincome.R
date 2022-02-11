
# setwd(Sys.getenv('TM_DIRECTORY'))

library(RUnit)
source('fixedincome.R')

holidays <- as.Date(unlist(read.table('ANBIMA.cal')))
cal <- calendar(holidays)

test.Calendar <- function() {
	checkEquals(class(cal), 'list')
}

test.workdays_between <- function() {
	d <- cal$workdays_between(as.Date('2001-01-01'), as.Date('2001-01-31'))
	checkEquals(d, 21)
	d <- cal$workdays_between(as.Date('2002-01-01'), as.Date('2002-01-31'))
	checkEquals(d, 21)
	d <- cal$workdays_between(as.Date('2002-01-02'), as.Date('2002-01-31'))
	checkEquals(d, 21)
	d <- cal$workdays_between(as.Date('2002-01-02'), as.Date('2002-02-01'))
	checkEquals(d, 22)
}

# setwd(Sys.getenv('TM_DIRECTORY'))
# source('fixedincome.R')
# holidays <- as.Date(unlist(read.table('ANBIMA.cal')))
# system.time(replicate(100, Calendar(holidays)))

# date_number <- function (y, m, d) {
# 	m <- (m + 9) %% 12
# 	y <- y - m%/%10
# 	return(365*y + y%/%4 - y%/%100 + y%/%400 + (m*306 + 5)%/%10 + (d-1))
# }
# g(2079, 01, 01) - g(2001, 01, 01)
# find_last_date <- function (date) as.Date(ISOdate(as.POSIXlt(date)$year, 1, 1)) - 1
# setwd(Sys.getenv('TM_DIRECTORY'))
# holidays <- as.Date(unlist(read.table('ANBIMA.cal')))
# dates <- seq(from=min(holidays), to=max(holidays), by='day')
# is.workday <- vapply(dates, function(.) {
# 	w <- as.POSIXlt(.)$wday
# 	return(! (w == 6 || w == 0 || any(. == holidays)))
# }, 0)
# idx <- 0
# index <- vapply(is.workday, function(.) {
# 	idx <<- idx + ifelse(., 1, 0)
# 	return(idx)
# }, 0)
# cal <- data.frame(dates, is.workday, index)
# head(cal)
# 

# dates.lt <- as.POSIXlt(dates)
# cc <- as.list(cbind(dates, dates.lt))
# head(cc)
# cal <- data.frame(date=dates, date.POSIX=dates.lt)
# head(cal)
# str(cal)
# cal$wday <- vapply(cal$date.POSIX, function(.) {
# 	print(class(.))
# 	stop('')
# })
# cal[,'wday'] <- vapply(dates.lt, function(.) {
# 	print(class(.))
# 	.$wday
# }, as.POSIXlt('1970-01-01'))
# head(cal)
