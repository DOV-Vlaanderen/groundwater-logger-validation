Logger <- function(name) {

  files <- list.files('./../../data/', recursive = TRUE, full.names = TRUE, pattern = ".*\\.csv")

  logger_path <- files[grep(sprintf('.*%s.*', name), files)]
  if (length(logger_path) > 1L) stop(sprintf("Multiple matches found for %s", name))
  if (length(logger_path) == 0L) stop(sprintf("No data found for logger %s", name))

  df <- data.table::fread(logger_path, dec = ",")
  df[, DRME_OCR_UTC_DTE := as.POSIXct(gsub("(.*):", "\\1", DRME_OCR_UTC_DTE),
                                      format = "%d/%m/%Y %H:%M:%S %z", tz = 'UTC')]
  df[, SEQUENCE := 1:.N]

  list(name = name,
       path = logger_path,
       df = data.table::copy(df))
}

get_loggers <- function(partner = c('inbo'), full_names = FALSE) {
  partner <- match.arg(partner)
  root <- sprintf("./../../data/raw/%s/", partner)
  list.files(root, full.names = full_names, pattern = ".*\\.csv")
}

aggregate_ts <- function(x, ts) {
  if(!(inherits(ts, "POSIXct") | inherits(ts, "Date"))) stop('ERROR: ts must either be POSIXct or Date.')

  stats::aggregate(x, by = list(as.Date(ts)), FUN = mean)$x
}
