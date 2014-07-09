# fixedincome

Tools for fixed income calculations.

To declare an annual spot rate with a `simple` compounding and an `actual/360` day count.

```r
as.spotrate(0.06, as.compounding('simple'), as.daycount('actual/360'))
# 0.06 simple actual/360 
```

Compound the spot rate for 7 months.

```r
sr <- as.spotrate(0.06, as.compounding('simple'), as.daycount('actual/360'))
compound(sr, '7 months')
# [1] 1.035
```

Spot rates can be created using a string representation

```r
as.spotrate('0.06 discrete actual/365')
# 0.06 discrete actual/365
```

### Pricing bonds

```r
dc <- as.daycount('actual/360')
comp <- as.compounding('continuous')
days <- c(97, 242, 321)
sr <- as.spotrate(rep(0.06, length(days)), comp, dc)
bonds <- data.frame(DaysToMaturity=days, Rate=sr, Notional=100000)
bonds
#   DaysToMaturity                              Rate Notional
# 1             97 0.06 discrete business/252 ANBIMA    1e+05
# 2            242 0.06 discrete business/252 ANBIMA    1e+05
# 3            321 0.06 discrete business/252 ANBIMA    1e+05
within(bonds, {
	PV <- Notional*discount(Rate, DaysToMaturity)
})
#   DaysToMaturity                       Rate Notional       PV
# 1             97 0.06 continuous actual/360    1e+05 98396.33
# 2            242 0.06 continuous actual/360    1e+05 96046.92
# 3            321 0.06 continuous actual/360    1e+05 94790.59
```

### Pricing bonds using a calendar

```r
library(bizdays)
cal <- Calendar(holidays=holidaysANBIMA, name='ANBIMA', weekdays=c('saturday', 'sunday'))
dc <- as.daycount('business/252')
comp <- as.compounding('discrete')
dates <- as.Date(c('2014-09-07', '2015-03-07', '2015-09-07'))
sr <- as.spotrate(rep(0.06, length(dates)), comp, dc, cal)
bonds <- data.frame(RefDate=as.Date('2014-03-21'), Maturity=dates, Rate=sr, Notional=100000)
bonds
#      RefDate   Maturity                              Rate Notional
# 1 2014-03-21 2014-09-07 0.06 discrete business/252 ANBIMA    1e+05
# 2 2014-03-21 2015-03-07 0.06 discrete business/252 ANBIMA    1e+05
# 3 2014-03-21 2015-09-07 0.06 discrete business/252 ANBIMA    1e+05
within(bonds, {
	PV <- Notional*discount(Rate, from=RefDate, to=Maturity)
})
#      RefDate   Maturity                              Rate Notional       PV
# 1 2014-03-21 2014-09-07 0.06 discrete business/252 ANBIMA    1e+05 97353.43
# 2 2014-03-21 2015-03-07 0.06 discrete business/252 ANBIMA    1e+05 94558.01
# 3 2014-03-21 2015-09-07 0.06 discrete business/252 ANBIMA    1e+05 91842.86
```

- new names:
	- spotrate
	- zerocurve
	- fwdcurve
	- indexcurve
	- historicaldata: zoo
- create 30/360 daycount
- create spot rate curve
- append rates to an spot rate curve
- set interpolation to curve, as an attribute: why interpolate explicitly when the curve knows which interpolation it should use
- vectorized operations for SpotRate functions
- functions to sum a curve
- function to shift a curve
- plot interest rate
- plot spot rate curve with interpolated points
- as.data.frame.spotratecurve
- other interpolation methods
	- Linear
	- LogLinear
	- Monotone Cubic Spline (Hyman)
	- Hermite
	- Natural Spline
	- Constrained Spline
- create an interp class
	- implement print method
	- interp is a spotratecurve attribute
- length method for spotratecurve
- The spotratecurve should have the same parameters as spotrate
- spotratecurve should have a name
- spotratecurve should be sliced
	- head(src, n)
	- tail(src, n)
	- CHECK src[1:10]
	- src[1:10, offset=1] -> daily forward curve
	- how to compute forward curves
- A SpotRate should be removed from a SpotRateCurve: curve[-10]
	- Not so simple: curve[n] returns a rate
	- curve[-n] should return a SpotRateCurve without the removed SpotRate
	- So this doesn't fit, different returns for the same operation
	- Proposal: curve[n] <- NA or curve[n] <- NULL
- SpotRateCurve should be a matrix, terms would be rownames, rates values and the rest attributes, like xts
	- dim
	- dimnames(curve) <- list(terms, curve_name)
	- ???
- Need a price curve: indexcurve, pricecurve


![](interestrateRpackage.gif)

