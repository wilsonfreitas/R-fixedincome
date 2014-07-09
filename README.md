# fixedincome



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

