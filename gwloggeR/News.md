# Future version

## General
* Caching of algorithm results.
* Use travis-ci for website building and build testing.
* Code documentation

## Diagnostic plots
* Show tresholds on diagnostic plots for differences(x).

# Development version

## Website
* gwloggeR website: https://dov-vlaanderen.github.io/groundwater-logger-validation/gwloggeR
* Getting started tutorial: https://dov-vlaanderen.github.io/groundwater-logger-validation/gwloggeR/docs/articles/gwloggeR.html 
* Hydrostatic pressure model explanation: https://dov-vlaanderen.github.io/groundwater-logger-validation/gwloggeR/docs/articles/Hydropressure.html

## Interface
* Hydrostatic pressure detect functions assert timestamps to be ordered.
* Hydrostatic pressure detect functions assert no duplicate timestamps.
* `detect_temporalchanges()` glags everything from start until the decay is below 5.

## Diagnostic plots
* Hydrostatic pressure x-axis label now shows correctly 'timestamps' instead of 'sequence'.
* Margin of diagnostic plots y-axis should be the same.
* TC events are now red dots and match the output vector TRUE values.
* LS events are now red dots and match the output vector TRUE values.

## General
* Completed `Development` vignette.
* Add date field in DESCRIPTION.

# gwloggeR 0.1.4

* `detect_outliers()`, `detect_levelshifts()` and `detect_temporalchanges()` for `gwloggeR::apriori("hydrostatic pressure")` data based on v0.06 (#35).
* Algorithm version numberering (#36).

# gwloggeR 0.1.3

* `detect_outliers()` function now has a `plot = TRUE` argument which prints diagnostic plots (#26).

# gwloggeR 0.1.0

* First version of gwloggeR.
