#' @keywords internal
aggregate <- function(x, ts) {
  if(!(inherits(ts, "POSIXct") | inherits(ts, "Date"))) stop('ERROR: ts must either be POSIXct or Date.')

  stats::aggregate(x, by = list(as.Date(ts)), FUN = mean)$x
}

