# Autocorrelation and ARIMA fit on KNMI data

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
  df[, 'FILE' := basename(logger.name)]
  df[, 'N' := .N]
  data.table::setkey(df, TIMESTAMP_UTC)
  structure(df, 'logger.name' = logger.name)
}

data.ref <- read.baro('KNMI_20200312_hourly')
data.ref

acf(data.ref$PRESSURE_VALUE, lag.max = 180)
spectrum(data.ref$PRESSURE_VALUE)
plot(x = data.ref[-(.N:(.N-124)), PRESSURE_VALUE], y = data.ref[-(1:125), PRESSURE_VALUE])

fit.arima <- forecast::auto.arima(data.ref$PRESSURE_VALUE, trace = TRUE)
plot(forecast:::simulate.Arima(fit.arima, 100))
arima.sim(model = fit.arima, n = 10, n.start = 5)
