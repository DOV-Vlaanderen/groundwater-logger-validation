## code to prepare `apriori-air-pressure` dataset goes here

files <- list.files('./data-raw/apriori-air-pressure/', full.names = TRUE)

airpressure <- lapply(files,  function(csv.file) {
  cols.selected <- c('DRME_ID', 'DRME_OCR_UTC_DTE', 'DRME_DRU')
  df <- data.table::fread(csv.file, dec = ",", select = cols.selected)

  # Filter
  df <- df[DRME_DRU < 1100,]
  df <- df[DRME_DRU > 800,]
  df <- df[!is.na(DRME_OCR_UTC_DTE)]

  # Standardized data columns
  df[, MEASUREMENT_ID := DRME_ID]
  df[, TIMESTAMP_UTC := as.POSIXct(gsub("(.*):", "\\1", DRME_OCR_UTC_DTE),
                                   format = "%d/%m/%Y %H:%M:%S %z", tz = 'UTC')]
  df[, PRESSURE_VALUE := DRME_DRU]
  df[, PRESSURE_UNIT := "cmH2O"]
  df[, (cols.selected) := NULL]

  # Aggregate by day to set all points on same frequency (i.e. weight)
  df <- df[, .(PRESSURE_VALUE = mean(PRESSURE_VALUE),
               PRESSURE_UNIT = unique(PRESSURE_UNIT)),
           by = .(TIMESTAMP_UTC = as.POSIXct(trunc(TIMESTAMP_UTC, units = 'days')))]

  # Meta data
  df[, FILE := basename(csv.file)]
  df[, N := .N]

  data.table::setkey(df, TIMESTAMP_UTC)

  df
})

airpressure <- data.table::rbindlist(airpressure, use.names = TRUE)

with(airpressure, {
  hist(PRESSURE_VALUE, breaks = 1000, probability = TRUE, main = '')
  curve(dnorm(x, mean = mean(PRESSURE_VALUE), sd = sqrt(var(PRESSURE_VALUE))), add = TRUE, col = 'red')
  curve(dnorm(x, mean = median(PRESSURE_VALUE), sd = robustbase::Qn(PRESSURE_VALUE)), add = TRUE, col = 'green')
})

with(airpressure, {
  qqnorm(PRESSURE_VALUE)
  qqline(PRESSURE_VALUE)
})

ggplot2::ggplot(data = airpressure, mapping = ggplot2::aes(y = PRESSURE_VALUE, x = paste(FILE, "-", N))) +
  ggplot2::geom_boxplot() +
  ggplot2::coord_flip()

median(airpressure$PRESSURE_VALUE)
robustbase::Qn(airpressure$PRESSURE_VALUE)

usethis::use_data(airpressure, overwrite = TRUE)
