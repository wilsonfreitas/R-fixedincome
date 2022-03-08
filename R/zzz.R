
.onAttach <- function(libname, pkgname) {
  if (requireNamespace("bizdays", quietly = TRUE)) {
    # check if bizdays has actual calendar
    if (!"actual" %in% names(calendars())) {
      bizdays::create.calendar("actual")
      packageStartupMessage("Created \"actual\" calendar")
    }
  }
}