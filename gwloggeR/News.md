# Development version

* gwloggeR website: https://dov-vlaanderen.github.io/groundwater-logger-validation/gwloggeR

## TODO
* `detect_temporalchanges()` should flag everything from start until the decay is insignificant.
* Complete `Development` vignette.

### Diagnostic plots
* Margin of diagnostic plots y-axis should be the same.
* Show tresholds on diagnostic plots for differences(x)
* Hydrostatic pressure x-axis label 'sequence' instead of 'timestamps'.

# gwloggeR 0.1.4

* `detect_outliers()`, `detect_levelshifts()` and `detect_temporalchanges()` for `gwloggeR::apriori("hydrostatic pressure")` data based on v0.06 (#35).
* Algorithm version numberering (#36).

# gwloggeR 0.1.3

* `detect_outliers()` function now has a `plot = TRUE` argument which prints diagnostic plots (#26).

# gwloggeR 0.1.0

* First version of gwloggeR.
