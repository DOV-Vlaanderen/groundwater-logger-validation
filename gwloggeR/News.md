# Future version

## General
* Caching of algorithm results.
* Use travis-ci for website building and build testing.
* Code documentation
* Set minimal R version to 3.2.5 (cf. branch r3.2.5 for testing)

# gwloggeR 0.2.1

* Fixed compatibility with R3.2.5 and old data.table v1.9.6 and ggplot2 v2.2.1

# gwloggeR 0.2.0

* Issue fixed: #47.
* Interface: `detect_drift()` implementation based on #67 and #70.
* Documentation: [Air pressure: drift](https://dov-vlaanderen.github.io/groundwater-logger-validation/gwloggeR/docs/articles/Airpressure-Drift.html).
* Prebuild binaries so not a whole repository has to be downloaded for installation

# gwloggeR 0.1.5

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
* Margin of diagnostic plots y-axis is now the same.
* TC events are now red dots and match the output vector TRUE values.
* LS events are now red dots and match the output vector TRUE values.

## General
* Completed [Development](https://dov-vlaanderen.github.io/groundwater-logger-validation/gwloggeR/docs/articles/Development.html) vignette.
* Add date field in DESCRIPTION.

# gwloggeR 0.1.4

* `detect_outliers()`, `detect_levelshifts()` and `detect_temporalchanges()` for `gwloggeR::apriori("hydrostatic pressure")` data based on v0.06 (#35).
* Algorithm version numberering (#36).

# gwloggeR 0.1.3

* `detect_outliers()` function now has a `plot = TRUE` argument which prints diagnostic plots (#26).

# gwloggeR 0.1.0

* First version of gwloggeR.
