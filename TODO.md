
# fixedincome

## Classes

- `SpotRate`
- `ForwardRate`
- `SpotRateCurve`
- `Term`
- `Compounding`
- `Daycount`
- `Interpolation`

## TODO

### Documentation

- [x] Term
- [x] Compounding
  - [x] Compounding Methods
- [x] Daycount
- [x] SpotRate
- [x] SpotRateCurve
- [x] ForwardCurve
- [x] Interpolation

### Daycount

- [ ] create 30/360 daycount
- [ ] Other daycounts <https://en.wikipedia.org/wiki/Day_count_convention>
- [ ] use DayCount with Term
- [ ] Create function convert_term(dc, "10 days", "months") to help converting terms' units

### SpotRate

- [ ] compound and discount functions accept (character, character)
      date arguments discount(r, "2017-08-01", "2017-08-21")

### SpotRateCurve

- [x] A spotrate should be removed from a spotratecurve: curve[-10] - curve[-n]
    should return a spotratecurve without the removed spotrate
- [ ] spotratecurve indexing:
    - [x] positional indexing: `curve[1]` for the first term, `curve[2]` for the second and so on.
    If the given index exceeds the curve length this operation returns `NA`.
    - [x] replacement with positional index `curve[1] \<- 0.01`
    - [x] associative indexing `curve[["1 day"]]` for `1 day`, `curve[["2 days"]]` for `2 days`, ...
    - [x] If the spotratecurve doesn't have a `2 days` index it returns NA
    - [x] removal of associative index
    - [x] replacement with associative index
    - [ ] date indexes
      - [ ] select
      - [ ] remove
      - [ ] replace
    - [x] term indexes
      - [x] select
      - [ ] remove
      - [x] replace
    - [x] sum curve to: numeric, curve
    - [ ] append SpotRateCurve
    - [ ] concatenate SpotRateCurve
- [ ] Naming: SpotRateCurve should have a name? I am not sure about that!
- [ ] create DiscountCurve - interpolations should be applied to it.
- [ ] arithmetic operations between curves respecting terms

### Multi SpotRateCurve

- [ ] Data structure for multiple SpotRateCurve objects, like a matrix.
      Useful for historical data and to build historical risk factors.

### ForwardRate

- [x] create forward rates
- [x] daily forward rates with interpolated data
- [ ] create a method for daily forward rates with interpolated data


### Interpolation

[ref1]: https://github.com/werleycordeiro/Dynamic-Nelson-Siegel-and-Svensson/blob/master/DNS-TS.R
[ref2]: https://www.r-bloggers.com/2021/05/dynamic-nelson-siegel-model-with-r-code/
[ref3]: https://guilhermecruzsp.wordpress.com/2022/04/19/interest-rate-model-simulations-vasicek/

- [x] set interpolation to SpotRateCurve
- [x] unset interpolation to SpotRateCurve
- [x] Interpolation will be used with associative indexing.
      If interpolation is set and a missing index is passed, then this value is interpolated.
- [ ] Methods
    - [x] FlatForward
    - [x] Linear
    - [x] LogLinear
    - [x] Monotone Cubic Spline (Hyman)
    - [x] Hermite
    - [x] Natural Spline
    - [x] Nelson-Siegel
    - [x] Nelson-Siegel-Svensson
    - [x] FlatForward COPOM
    - [ ] Constrained Spline
    - [ ] Dynamic Nelson-Siegel [ref1][ref1], [ref2][ref2]
    - [ ] Vasicek (Haug 11.4.2) [ref][ref3]
    - [ ] Rendleman and Bartter (Haug 11.4.1)
    - [ ] Ho and Lee (Haug 11.4.3)
    - [ ] Hull and White (Haug 11.4.4)
    - [ ] Black-Derman-Toy (Haug 11.4.5)
- [ ] Improve curve fit stop using object copy
- [ ] Work with terms in years instead of days

### Bonds

- [ ] define bonds
- [ ] pricing bonds

### Bootstrap

- [ ] bonds bootstrap
- [ ] Curve models: Nelson-Siegel, Diebold-Li, Nelson-Siegel-Svensson

### Other classes

- [ ] IndexCurve - for inflation index or IDI index, for example - rbcb integration
- [ ] HistoricalRates - for CDI historical data, for example - rbcb integration

### Plotting

- [x] SpotRateCurve: points and lines
- [x] SpotRateCurve: lines with daily interpolated points
- [x] ForwardRate curve with steps
- [x] daily ForwardRate curve with interpolated points
- [ ] plot multiple SpotRateCurve objects
- [x] use daycount to create x-axis ticks

### Other issues

- [ ] IR functions: <https://support.google.com/docs/table/25273?hl=en>
- [ ] Risk measures: YTM, Duration, Convexity
- [ ] short rate models: Vasicek, CIR, ...

### Register

- [ ] naming objects and register (aka container)
    - Objects like HistoricalRates, SpotRateCurve, IndexCurve and Calendar should be referenced by their names.
      These objects are used by many others and their copies should imply unnecessary memory use.
    - Integrate with rbcb package to transform series into IndexCurve or HistoricalRates.
    - Methods which work with these objects should work with characters that represent the objects.

---

## Legacy

Methods to be implemented in the term class:

-   initialize
-   show
-   format.S3
-   xtfrm.S3
-   c
-   rep
-   [, [[ (x,i, j, ...)
-   [\<-, [[\<- (x,i, j, ..., value)
-   \$, \$\<- (x, name, value)
-   summary.S3
-   Arith
    -   (., ANY), (ANY, .)
-   Compare
    -   (., character), (character, .)
    -   (., numeric), (numeric, .)
-   as.term
    -   character
    -   numeric

Bonds can be created using a `data.frame` and `spotrate` can be set as
one of columns.

```r
# Declaring bonds
dc <- as.daycount('actual/360') # daycount rule
comp <- as.compounding('continuous') # compounding regime
days <- c(97, 242, 321) # days to maturity
sr <- as.spotrate(rep(0.06, length(days)), comp, dc) # discount rate

bonds <- data.frame(DaysToMaturity=days, Rate=sr, Notional=100000)
bonds
#   DaysToMaturity                              Rate Notional
# 1             97 0.06 discrete business/252 ANBIMA    1e+05
# 2            242 0.06 discrete business/252 ANBIMA    1e+05
# 3            321 0.06 discrete business/252 ANBIMA    1e+05

# Pricing bond -- discounting their notional value
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
