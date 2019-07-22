## code to prepare `apriori-air-pressure` dataset goes here

files <- list.files('./data-raw/apriori-air-pressure/', full.names = TRUE)

df <- lapply(files,  function(csv.file) {
  cols.selected <- c('DRME_ID', 'DRME_OCR_UTC_DTE', 'DRME_DRU')
  df <- data.table::fread(csv.file, dec = ",", select = cols.selected)

  # Meta data
  df[, FILE := basename(csv.file)]
  df[, N := .N]

  # Filter
  df <- df[DRME_DRU < 1100,]
  df <- df[DRME_DRU > 800,]

  # Standardized data columns
  df[, MEASUREMENT_ID := DRME_ID]
  df[, TIMESTAMP_UTC := as.POSIXct(gsub("(.*):", "\\1", DRME_OCR_UTC_DTE),
                                   format = "%d/%m/%Y %H:%M:%S %z", tz = 'UTC')]
  df[, PRESSURE_VALUE := DRME_DRU]
  df[, PRESSURE_UNIT := "cmH2O"]
  df[, (cols.selected) := NULL]

  data.table::setkey(df, MEASUREMENT_ID)

  df
})

df <- data.table::rbindlist(df, use.names = TRUE)

hist(as.integer(diff(df$TIMESTAMP_UTC)), breaks = 100)

hist(df$PRESSURE_VALUE, breaks = 1000, probability = TRUE, main = '')
curve(dnorm(x, mean = mean(df$PRESSURE_VALUE), sd = sqrt(var(df$PRESSURE_VALUE))), add = TRUE, col = 'red')

qqnorm(df$PRESSURE_VALUE)
qqline(df$PRESSURE_VALUE)

ggplot2::ggplot(data = df, mapping = ggplot2::aes(y = PRESSURE_VALUE, x = paste(FILE, "-", N))) +
  ggplot2::geom_boxplot() +
  ggplot2::coord_flip()

median(df$PRESSURE_VALUE)
robustbase::Qn(df$PRESSURE_VALUE)

# usethis::use_data("apriori-air-pressure")
