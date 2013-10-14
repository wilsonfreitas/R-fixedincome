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
- length method for SpotRateCurve
- indexation `[<-` should allow updating
- `interp` function should accept a parameter `method` to define the interpolation method to use, if it is `NULL` the curve's default is used.
- The SpotRateCurve should have the same parameters as SpotRate
- SpotRateCurve should have a get.SpotRate method (or something alike)
- The SpotRateCurve `insert` should check if the SpotRate's parameters are compatible with SpotRateCurve's parameters (parameters like `dib` and `compounding`)
- SpotRateCurve should have a name
- SpotRateCurve should be sliced
	- head(src, n)
	- src[1:10]
	- src[1:10, offset=1] -> daily forward curve
- A SpotRate should be removed from a SpotRateCurve: curve[-10]

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
