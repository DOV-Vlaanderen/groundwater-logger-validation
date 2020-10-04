# Simulation study: drifting AR(1) model

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

  # Meta data
  df[, 'FILE' := basename(logger.name)]
  df[, 'N' := .N]

  data.table::setkey(df, TIMESTAMP_UTC)
  data.table::setattr(df, 'logger.name', logger.name)

  df
}

# from analysis 04
compare <- function(df1, df2) {
  if (nrow(df1) == 0L || nrow(df2) == 0L) return(data.table::data.table())

  diff.df <- df1[J(df2), .('PRESSURE_DIFF' = x.PRESSURE_VALUE - i.PRESSURE_VALUE, TIMESTAMP_UTC)][!is.na(PRESSURE_DIFF), ]

  data.table::setkey(diff.df, TIMESTAMP_UTC)

  diff.df
}

ref.compare <- function(df) {
  compare(df, read.baro('KNMI_20200312_hourly'))
}

# plot(arima.sim(n = 10000, list(ar = c(0.9)), sd = sqrt(23.62)) + 1033.64)
# plot(forecast:::simulate.Arima(fit.arima, 10000))

df.drifter <- data.table::data.table(
  TIMESTAMP_UTC = seq(from = as.POSIXct('2010-01-01 00:00:00'), to = as.POSIXct('2020-01-01 00:00:00'), by = '12 hours'),
  PRESSURE_VALUE = arima.sim(n = 7305, list(ar = c(0.9)), sd = sqrt(23.62)) + 1033.64 + c(rep(0, 3000), 1:4305)*0.005,
  key = 'TIMESTAMP_UTC'
)

arima(df.drifter$PRESSURE_VALUE, order = c(1, 0, 0))
plot(df.drifter$PRESSURE_VALUE)

df.diff <- ref.compare(df.drifter)
plot(df.diff$PRESSURE_DIFF)
