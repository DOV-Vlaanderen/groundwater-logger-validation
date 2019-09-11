# See: https://cran.r-project.org/web/packages/roxygen2/vignettes/rd.html
# Keyword "internal" is recommended for package documentation.

#' @details
#' TODO: package details.
#' @keywords internal
#' @import stats
#' @docType package
#' @references \href{https://github.com/DOV-Vlaanderen/groundwater-logger-validation/tree/master/gwloggeR}{GitHub}
"_PACKAGE"
#> [1] "_PACKAGE"

# NSE variables that cause CRAN R CMD check notes...
# Very ugly, but the solution is suggested by CRAN
# See: https://stackoverflow.com/q/9439256/1548942
utils::globalVariables(c('J', '.', '.N')) # data.table internals
utils::globalVariables(c('.valueClassTest')) # S4 without full methods import
utils::globalVariables(c('TIMESTAMP_UTC', 'FILE', 'PRESSURE_VALUE'))
utils::globalVariables(c('DIFF.SEC', 'CUM.DIFF.SEC', 'DIFF.VALUE'))
