# Best ARIMA model for barometer difference with KNMI data

logger.names <- grep('barometer/', gwloggeR.data::enumerate(), value = TRUE)

logger.names <- setdiff(logger.names, 'barometer/BAOL016X_W1666.csv')
logger.names <- setdiff(logger.names, 'barometer/BAOL050X_56819.csv') # high freq manu in range of 80 cmH2O

round_timestamp <- function(ts, scalefactor.sec = 3600*12) {
  as.POSIXct(round(as.numeric(ts)/scalefactor.sec) * scalefactor.sec, origin = '1970-01-01', tz = 'UTC')
}

read.baro <- function(logger.name) {
  df <- gwloggeR.data::read(logger.name)$df
  if (nrow(df) == 0L) return(df)
  df <- df[!is.na(TIMESTAMP_UTC), ]
  df <- df[!is.na(PRESSURE_VALUE), ]
  df <- df[!duplicated(TIMESTAMP_UTC), ]
  df <- df[, .('PRESSURE_VALUE' = mean(PRESSURE_VALUE)),
           by = .('TIMESTAMP_UTC' = round_timestamp(TIMESTAMP_UTC))]

  # Filter
  df <- df[PRESSURE_VALUE < 1100,]
  df <- df[PRESSURE_VALUE > 975,]

  # Meta data
  df[, 'FILE' := basename(logger.name)]
  df[, 'N' := .N]

  data.table::setkey(df, TIMESTAMP_UTC)
  data.table::setattr(df, 'logger.name', logger.name)

  df
}

# from analysis 04, with  normalization
compare <- function(df1, df2) {
  if (nrow(df1) == 0L || nrow(df2) == 0L) return(data.table::data.table())

  diff.df <- df1[J(df2), .('PRESSURE_DIFF' = x.PRESSURE_VALUE - i.PRESSURE_VALUE,
                           TIMESTAMP_UTC, 'FILE' = x.FILE)][!is.na(PRESSURE_DIFF), ]

  # Normalization: makes sure the AR(1) component is not influenced by
  # differences in height.
  diff.df[, PRESSURE_DIFF := PRESSURE_DIFF - median(PRESSURE_DIFF)]

  data.table::setkey(diff.df, TIMESTAMP_UTC)

  diff.df
}

ref.compare <- function(logger.name, df = read.baro(logger.name)) {
  compare(df, read.baro('KNMI_20200312_hourly'))
}

# all barometers
df <- data.table::rbindlist(lapply(logger.names, ref.compare), use.names = TRUE, fill = TRUE)
data.table::setkey(df, FILE, TIMESTAMP_UTC)
df
ggplot2::ggplot(data = df, ggplot2::aes(x = TIMESTAMP_UTC, y = PRESSURE_DIFF, color = FILE)) +
  ggplot2::geom_line(show.legend = FALSE)
arima(df$PRESSURE_DIFF, order = c(1, 0, 0))
# Here the most impact has the AR(1) model, with relative big impact of AR(2) also.
# The AR(1) coefficient is here 0.83 at 12h intervals with sigma2 = 5.95.


# 65% non-drinfting barometers
analysis_10 <- data.table::fread(file = './drifts/analysis_10/baro_median_errors.csv')
analysis_10 <- analysis_10[order(WISK, decreasing = TRUE),]
dfw <- data.table::rbindlist(
  lapply(analysis_10[WISK <= quantile(WISK, probs = 0.65), FILE], ref.compare),
  use.names = TRUE, fill = TRUE
)
data.table::setkey(dfw, FILE, TIMESTAMP_UTC)
dfw
ggplot2::ggplot(data = dfw, ggplot2::aes(x = TIMESTAMP_UTC, y = PRESSURE_DIFF, color = FILE)) +
  ggplot2::geom_line(show.legend = FALSE)
arima(dfw$PRESSURE_DIFF, order = c(1, 0, 0))
# Here the most impact has the AR(1) model, after that the impact is not large.
# The AR(1) coefficient is here only 0.54 at 12h intervals with sigma2 = 2.13!
# That AR(1) is only 0.54 is not expected. According to theory, it should be the
# same as for the original barometer data, wich is around 0.88 for 12h intervals.

# The peculiar thing is that if I do not normalize the data, the coef is 0.89 with
# sigma2 = 2.625.

