
# gwloggeR

R package to identify problematic observations from groundwater logger data

## Installation

As the `gwloggeR` package is not available on CRAN, the easiest way of installing the package is using the `devtools` package and referring to the git repository holding the package code:

```
devtools::install_github("DOV-Vlaanderen/groundwater-logger-validation", subdir = "gwloggeR")
```

Remark that the package itself is stored in a subfolder of the repository, which is tackled by the `subdir` command.

One installed, the functionalities can be derived by loading the package:

``` r
library("gwloggeR")
```
