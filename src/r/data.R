get_loggers <- function(partner = c('inbo'), full_names = FALSE, pattern = ".*\\.csv") {
  partner <- match.arg(partner)
  root <- sprintf("./../../data/raw/%s/", partner)
  list.files(root, full.names = full_names, pattern = pattern)
}

aggregate_ts <- function(x, ts) {
  if(!(inherits(ts, "POSIXct") | inherits(ts, "Date"))) stop('ERROR: ts must either be POSIXct or Date.')

  stats::aggregate(x, by = list(as.Date(ts)), FUN = mean)$x
}
