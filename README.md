# fixedincome

- CHECK create interest rate curve
- CHECK append rates to an interest rate curve
- InterestRate class
	- rate
	- Compounding
	- DayCount
	- Frequency
- CHECK implement indexes
- CHECK test curve size
- CHECK test curve's rates and terms sizes
- CHECK set interpolation to curve, as an attribute: why interpolate explicitly when the curve knows which interpolation it should use
- vectorized operations for SpotRate functions
- functions to sum a Curve
- function to shift a curve
- CHECK plot interest rate
- plot spot rate curve with interpolated points
- CHECK as.data.frame.SpotRateCurve
- other interpolation methods
	- CHECK Linear
	- CHECK LogLinear
	- CHECK Monotone Cubic Spline (Hyman)
	- CHECK Hermite
	- CHECK Natural Spline
	- Constrained Spline
- Figure out a way of defining flat curve
- CHECK length method for SpotRateCurve
- CHECK indexation `[<-` should allow updating
- FORGET `interp` function should accept a parameter `method` to define the interpolation method to use, if it is `NULL` the curve's default is used.
- CHECK The SpotRateCurve should have the same parameters as SpotRate
- CHECK SpotRateCurve should have a get.SpotRate method (or something alike)
- FORGET The SpotRateCurve `insert` should check if the SpotRate's parameters are compatible with SpotRateCurve's parameters (parameters like `dib` and `compounding`)
- SpotRateCurve should have a name
- SpotRateCurve should be sliced
	- head(src, n)
	- CHECK src[1:10]
	- FORGET src[1:10, offset=1] -> daily forward curve
- A SpotRate should be removed from a SpotRateCurve: curve[-10]
	- Not so simple: curve[n] returns a rate
	- curve[-n] should return a SpotRateCurve without the removed SpotRate
	- So this doesn't fit, different returns for the same operation
	- Proposal: curve[n] <- NA or curve[n] <- NULL
- SpotRateCurve should be a matrix, terms would be rownames, rates values and the rest attributes, like xts
	- dim
	- dimnames(curve) <- list(terms, curve_name)
	- ???
- Curve interpolation should return a SpotRateCurve or a CurveInterpolation
	curve <- CurveInterpolation(curve, method='flatforward')
	expect_equal( curve[c(1, 11, 14, 21)], c(0.0719, 0.056, 0.060220003, 0.065400693) )
	It is tough or even unnecessary call SpotRateCurve and call CurveInterpolation …
	CurveInterpolation(SpotRateCurve(rates, terms,
	    dib=252, compounding='compounding'))
- Flat forward interpolation should use the compounding curve’s attribute instead of assuming compounded rates.
	interp=function(curve, term, interp.FUN) {
	    log.price <- interp.FUN(term)
	    price <- exp(log.price)
	    price^(dib(curve)/term) - 1
	},
- For a given curve with a defined compounding, create a new curve with different compounding, or even different settings as dib and compounding.
- The intep.method should always be provided, instead of method only. This makes the constructor more flexible, avoiding inner and hidden behaviors.



#### try

		as.SpotRateCurve.data.frame <- function(df, ...) {
		    SpotRateCurve(terms=df$terms, rates=df$rates, ...)
		}


![](interestrateRpackage.gif)


		interp.Linear <- function(object, ...) UseMethod('interp.Linear', object)
		interp.LogLinear <- function(object, ...) UseMethod('interp.LogLinear', object)
		interp.CubicSpline <- function(object, ...) UseMethod('interp.CubicSpline', object)
		interp.Linear.SpotRateCurve <- function(curve, term) {
		    if ( any( idx <- curve$terms == term ) ) {
		        return(curve$rates[idx])
		    } else {
		        idx.u <- min(which(curve$terms > term))
		        idx.d <- max(which(curve$terms < term))
		        ir.u <- SpotRate(curve$rates[idx.u], curve$terms[idx.u])
		        ir.d <- SpotRate(curve$rates[idx.d], curve$terms[idx.d])
		        ir.fwd <- forward.rate(ir.d, ir.u)
		        # use forward rate with new term
		        ir.fwd.adj <- as.SpotRate(ir.fwd, term-term(ir.d))
		        new.cf <- as.CompoundFactor(ir.d) * as.CompoundFactor(ir.fwd.adj)
		        return( rate(as.SpotRate(new.cf)) )
		    }
		}
		print.SpotRateCurve <- function(curve) {
		}



## Group Methods - Math

In fact most of R’s mathematical functions would require an
almost identical deﬁnition. Fortunately, there is a short-hand
way of deﬁning all the methods with one function deﬁnition.

	Math.vcoords = function(x)
		vcoords(xcoords(x),
		ycoords(x),
		get(.Generic)(values(x)))

The expression get(.Generic) gets the function with the
name that Math.vcoords was invoked under.
