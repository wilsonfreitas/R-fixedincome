## Resubmission of an archived package

fixedincome was archived on 2024-08-25 because it did not use the suggested
package 'rb3' conditionally. This version fixes that: 'rb3' is no longer in
Suggests and the vignettes don't run 'rb3' code (they show it with
`eval = FALSE` and load saved data from `vignettes/*.rds`).

## R CMD check results

0 errors | 0 warnings | 1 note

* New submission / Package was archived on CRAN: see above.

## Test environments

* Local: Ubuntu, R 4.5.2
* GitHub Actions: Ubuntu (R-devel, release, oldrel-1), Windows (release),
  macOS (release)

## Reverse dependencies

There are no reverse dependencies on CRAN.
