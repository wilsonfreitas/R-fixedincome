# fixedincome

Calculations involving interest rates are usually very easy and straightforward, but sometimes it involves specific issues that makes the task of writing structured and reprodicible code for it chalenging and annoying.
The `fixedincome` package brings many functions to strucutre and create facilities to handle with interest rates, term structure of interest rates and specific issues regarding compounding rates and day count rules, for example.

Below there are a few examples on how to create and make calculations with interest rates using `fixedincome`.

## Examples

To create an interest rate we need to specify 4 elements:

- the value of the interest rate itself, a decimal number
- the compounding regime of interest rate, that can be `simple`, `discrete` or `continuous` and we'll use `simple`.
- the day count rule which defines how interest is accrued over time, we have a few options and we'll start with `actual/360` where the days between two dates are calculated as the difference and the year is assumed to be 360 days.
- the calendar used to count the number of days between two dates of accrual, we will use the `actual` calendar that compute the Julian difference between two dates.

There is another important topic that wasn't declared here that is the *frequency* of interest.
To make things simple `fixedincome` handles only with *annual* rates since this represents the great majority of rates used in financial market contracts, but this restriction can be reviewed in the future.

Given that let's declare an annual spot rate with a `simple` compounding, an `actual/360` and the `actual` calendar.

```r
sr <- spotrate(0.06, 'simple', 'actual/360', 'actual')
sr
# simple actual/360 actual 
# 0.06
```

Compound the spot rate for 7 months.

```r
compound(sr, 7, 'months')
# [1] 1.035
```

Spot rates can be created using a string representation

```r
as.spotrate('0.06 discrete actual/365 actual')
# 0.06 discrete actual/365
```

### Spot rate curves

Let's create a spot rate curve.

```r
curve <- spotratecurve(c(1, 11, 26, 47, 62),
                       c(0.0719, 0.056, 0.0674, 0.0687, 0.07),
                       compounding='simple', daycount='actual/365',
                       units='days', name='TS')
curve
##             TS
##  1 days 0.0719
## 11 days 0.0560
## 26 days 0.0674
## 47 days 0.0687
## 62 days 0.0700
## simple actual/365 
```

```{r}
require(ggplot2)
ggplot(data=as.data.frame(curve), aes(x=terms, y=rates)) + geom_line() + geom_point()
```

![Spot Rate Curve](TS.png "Spot Rate Curve")


