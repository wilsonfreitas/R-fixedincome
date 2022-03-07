
.onAttach <- function(libname, pkgname) {
  suppressMessages(suppressWarnings(library(bizdays)))

  # check if bizdays has actual calendar
  if (!"actual" %in% names(calendars())) {
    bizdays::create.calendar("actual")
    message("Created \"actual\" calendar")
  }
}