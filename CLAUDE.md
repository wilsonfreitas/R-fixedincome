# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this is

`fixedincome` is an R package (on CRAN) for interest-rate math: spot rates, forward rates, term structures, compounding regimes, day count rules and curve interpolation. Business-day counting is delegated to the `bizdays` package (calendars such as `"Brazil/ANBIMA"` are loaded in `.onAttach`, see `R/zzz.R`).

## Commands

Run from the repo root in R (or `Rscript -e '...'`):

```r
devtools::load_all()                 # load package for interactive work
devtools::document()                 # regenerate man/ and NAMESPACE from roxygen
devtools::test()                     # run all tests (testthat, edition 3)
devtools::test(filter = "spotratecurve")   # single test file: tests/testthat/test-spotratecurve.R
testthat::test_file("tests/testthat/test-term.R")
devtools::check()                    # full R CMD check (what CI runs)
rmarkdown::render("README.Rmd")      # README.md is generated — edit README.Rmd
pkgdown::build_site()                # docs site (config in _pkgdown.yml)
```

CI (`.github/workflows/R-CMD-check.yaml`) runs `rcmdcheck` on macOS, Windows and Ubuntu (devel/release/oldrel-1).

## Architecture

Everything is **S4**. Core value classes extend base `numeric`, so they behave like vectors (work in `data.frame`/`tibble`, support `[`, `c()`, arithmetic) while carrying metadata in slots:

- `Term` (`numeric` + `units`) and `DateRangeTerm` (adds `start_date`, `end_date`, `calendar`) — `R/term-class.R`.
- `Compounding` is a virtual class with `Simple`, `Discrete`, `Continuous` subclasses — `R/compounding-class.R`. Compounding math lives in `R/compound-method.R` (`compound()`, `discount()` generics dispatching on rate/term/date combinations).
- `Daycount` (`character`, e.g. `"actual/360"`, `"business/252"`) — `R/daycount-class.R`.
- `SpotRate` (`numeric` + `compounding`, `daycount`, `calendar`) — `R/spotrate-class.R`. Arithmetic/comparison/`c()` between SpotRates require matching compounding, daycount and calendar, otherwise they error.
- `ForwardRate` extends `SpotRate` with terms — `R/forwardrate-class.R`.
- `SpotRateCurve` extends `SpotRate` with `terms`, `refdate`, `interpolation` — `R/spotratecurve-class.R`. Validity requires unique, positive, non-NA terms of same length as rates. `[` is positional indexing; `[[` is indexing **by term** and uses the interpolation when the term is not on the curve.
- `Interpolation` (`character` + `func`, `propagate`) with subclasses `FlatForward`, `Linear`, `LogLinear`, `NaturalSpline`, `HermiteSpline`, `MonotoneSpline`, and parametric `NelsonSiegel` / `NelsonSiegelSvensson` (parameters in slots) — `R/interpolation-class.R`.

Interpolation engine (`R/spotratecurve-interpolation.R`, `R/spotratecurve-interpolation-nelsonsiegel.R`): setting `interpolation(curve) <- interp_xxx()` calls the `prepare_interpolation(object, curve)` generic, which returns the Interpolation object with `func` filled in; the curve then evaluates `func` for missing terms. `fit_interpolation()` estimates parametric models (NS/NSS) from a curve. To add a new method: new subclass + constructor + `prepare_interpolation` method.

Plotting: base graphics in `R/plot.R`, ggplot2 in `R/autoplot.R`.

## Conventions and gotchas

- **`Collate:` in DESCRIPTION fixes file load order** (classes before methods that use them). A new `R/` file must be added there in the right position, or `load_all`/check fails. No `@include` tags are used.
- `NAMESPACE` and `man/` are roxygen-generated (markdown enabled) — never edit by hand; run `devtools::document()`.
- S4 methods that need a doc entry but would clutter the index get an alias in `R/hidden.R` (`hidden_aliases`). Add new method aliases there if `R CMD check` complains about undocumented S4 methods.
- Dataset `ZeroCurveBRL` (`data/`) is documented in `R/fixedincome-internal.R`.
- Vignettes don't download data: the rb3 (>= 0.1.0) code is shown with `eval=FALSE`, and a hidden chunk loads `vignettes/yc-<date>.rds` (made once from a local rb3 cache, because B3 no longer serves those 2022 files).
- Only annual rates are supported (no frequency concept).
- `examples/`, `resources/`, `images/` are scratch/legacy material excluded from the build (`.Rbuildignore`); they are not tests and may not run against the current API.
