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