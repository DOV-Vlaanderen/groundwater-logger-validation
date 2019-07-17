#' @keywords internal
aggregate <- function(x, ts, by = c('days', 'hours')) {
  if(!(inherits(ts, "POSIXct") | inherits(ts, "Date"))) stop('ERROR: ts must either be POSIXct or Date.')
  by <- match.arg(by)

  stats::aggregate(x, by = list(as.POSIXct(trunc(ts, units = by))), FUN = mean)$x
}

