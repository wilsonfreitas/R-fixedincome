# fixedincome 0.0.2

* Added `progagate` slot to `Interpolation` class. It defines if the interpolation method is propagated to interpolated curves.
* The curve methods `[` and `[[` don't propagate interpolation. The returned curves don't have interpolation. This avoids recurrent `prepare_interpolation` calls.
* The `interpolate` function, for NelsonSiegel and NelsonSiegelSvensson models, converts `term` argument to years with `toyears`.
* Organized nelsonsiegel and nelsonsiegelsvensson code.
  * New objective functions (and their gradients) have been created to improve the optimization.
* New SpotRateCurve method `[[` for missing returns interpolated curve in curve's terms.
* SpotRate Compare method split into spectific methods: ==, !=, >, <, >=, <=. The methods >, <, >=, <= raise error for objects with different slots: compounding, daycount, calendar.
* SpotRate Arith methods raise error for objects with different slots: compounding, daycount, calendar.
* `toyears` returns Term objects
* New methods `tomonths` and `todays` for Term conversion in different units.
* Updated to bizdays version 0.1.10 (Issue #21)
* Code refactoring

# fixedincome 0.0.1

* First CRAN version.