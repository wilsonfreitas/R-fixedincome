# fixedincome (development version)

* New daycount `30E/360` (Eurobond basis): every month has 30 days, and the 31st is moved to the 30th. `compound()` and `discount()` with dates count the days with this rule instead of the calendar. Other 30/360 variants are still rejected. (Issue #5)
* New vignette "Day Count Conventions".
* New `period` argument in `spotrate()`: rates for other periods, like a monthly inflation rate (`period = "1 month"`) or a rate for a fixed period between two dates (`period = term(d1, d2, calendar)`), are converted to the equivalent annual rate. The vignette "Day Count Conventions" shows how to compute real interest rates. (Issue #25)
* The error for operations between SpotRates with different slots says which slots: compounding, daycount or calendar.

# fixedincome 0.1.0

## Breaking changes

* `daycount()` rejects daycounts other than `actual/...` and `business/...`, such as `30/360`, which were silently computed as actual days. (Issue #5)
* Requires R >= 4.1.0.

## New features

* New `Bps` class for basis points: `bps(10)` is 0.001, `as.bps()` converts a rate difference, and `c()` combines them. Adding or subtracting `Bps` to a `SpotRate` or `SpotRateCurve` shifts the rates and keeps the other slots. (Issue #33)
* New `with_daycount()` evaluates operations between `Term` objects with different units using a daycount, e.g. `with_daycount("business/252", term(1, "month") + term(12, "days"))` gives 33 days. (Issue #27)
* Arithmetic (`+`, `-`, `%%`, `/`, `%/%`) and comparison between `Term` objects with the same units; before, any operation between two Terms raised "Not implemented". Terms with different units raise an error explaining how to convert them. (Issue #28)
* `forwardrate(curve, t1, t2)` accepts numeric or `Term` vectors and uses the curve's interpolation. (Issue #17)
* New vignette "Working with Terms", and a section about basis points in "Spot Rate Curve Interpolation".

## Bug fixes

* `[[` and `[[<-` on `SpotRateCurve` convert `Term` objects to days before indexing. Before, `curve[[term(1, "years")]]` silently returned the rate at 1 day. (Issue #22)
* Arithmetic on a `SpotRateCurve` with an interpolation prepares the interpolation again; before, `curve + 0.01` kept returning the old rates from `[[`.
* `c()` on `Term` objects keeps the units of each element; before, all elements took the units of the first one.
* `[` on `SpotRateCurve` gives a clear error for NA or out of range indices. (Issue #24)
* Vignettes no longer run rb3 code or download data, and rb3 was removed from Suggests (the package was archived on CRAN because rb3 was not used conditionally). The rb3 code shown is updated to rb3 >= 0.1.0.

# fixedincome 0.0.5

* Maintenance release: updated GitHub Actions and documentation.

# fixedincome 0.0.4

* Improved nelsonsiegel and nelsonsiegelsvensson optimization.
* Corrected bug due to recent changes in `match` in R-devel

# fixedincome 0.0.3

* ggplot2 support.
* Added optional refdate attribute to ForwardRate class. (Issue #29)
* New vignette "Plotting Spot Rate Curves"
* Updated vignette "Spot Rate Curve Interpolation"

# fixedincome 0.0.2

## Term class

* Term class accepts multiple units.
* `toyears` returns Term objects
* New methods `tomonths` and `todays` for Term conversion in different units.

## SpotRate

* SpotRate Compare method split into spectific methods: ==, !=, >, <, >=, <=. The methods >, <, >=, <= raise error for objects with different slots: compounding, daycount, calendar.
* SpotRate Arith methods raise error for objects with different slots: compounding, daycount, calendar.

## SpotRateCurve

* The curve methods `[` and `[[` don't propagate interpolation. The returned curves don't have interpolation. This avoids recurrent `prepare_interpolation` calls.
* New SpotRateCurve method `[[` for missing returns interpolated curve in curve's terms.
* SpotRateCurve converts terms to `days` units, so `days` units becomes the default units to SpotRateCurve.

## Interpolation

* Added `progagate` slot to `Interpolation` class. It defines if the interpolation method is propagated to interpolated curves.
* The `interpolate` function, for NelsonSiegel and NelsonSiegelSvensson models, converts `term` argument to years with `toyears`.
* Organized nelsonsiegel and nelsonsiegelsvensson code.
  * New objective functions (and their gradients) have been created to improve the optimization.

## Compounding

* `rates` method has been renamed to `implied_rate`

## Misc

* Code refactoring
* Updated to bizdays version 0.1.10 (Issue #21)

# fixedincome 0.0.1

* First CRAN version.