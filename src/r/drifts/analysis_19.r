# Best ARIMA model for barometer data

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
  structure(df, 'logger.name' = logger.name)
}

df.list <- lapply(logger.names, read.baro)
df <- data.table::rbindlist(df.list, use.names = TRUE, fill = TRUE)
data.table::setkey(df, FILE, TIMESTAMP_UTC)
df

# Unit root test seems to fail and 1st order diff are taken.
# But assuming stationary one ends up with a slightly better likelihood and
# a much more parsimonious model.
forecast::auto.arima(df$PRESSURE_VALUE, trace = TRUE) # ARIMA(0,1,5): log likelihood=-1259788
forecast::auto.arima(df$PRESSURE_VALUE, trace = TRUE, stationary = TRUE) # ARIMA(2,0,0) with non-zero mean: log likelihood=-1256646

# The effect of the constant seems large:
# ARIMA(0,0,0) with zero mean     : 7033251
# ARIMA(0,0,0) with non-zero mean : 3221075
# Then the effect of the first AR(1) component also:
# ARIMA(1,0,0) with non-zero mean : 2523871
# The second AR component has hardly any effect:
# ARIMA(2,0,0) with non-zero mean : 2513298
# Note further that just taking differences has a huge impact:
# ARIMA(0,1,0)                    : 2545514
# which is much better than a white noise model: ARIMA(0,0,0)
# with non-zero mean, but slightly worse than ARIMA(1,0,0)
# with non-zero mean

# Gaussian noise model: too dense
fit.arima <- arima(df$PRESSURE_VALUE, order = c(0, 0, 0))
plot(forecast:::simulate.Arima(fit.arima, 10000))

# AR(1) model: looks quite like a real air pressure series
fit.arima <- arima(df$PRESSURE_VALUE, order = c(1, 0, 0))
plot(forecast:::simulate.Arima(fit.arima, 10000))

# Distribution of AR(1) model
# Seems to be normal given a timestamp.
# So according to this approximation, a height-compensated barometer should be
# within a normal distribution with mu = 1033 and sd = 11.137
# Note that assumption is that the barometers are height-compensated.
df.sim <- data.table::rbindlist(lapply(1:10000, function(x) data.frame(t = 1:1000, x = forecast:::simulate.Arima(fit.arima, 1000))))
data.table::setkey(df.sim, t, verbose = TRUE)
# plot(x = df.sim$t, y = df.sim$x, pch = '.')
df.sim.stats <- df.sim[, .(sd = sd(x)), by = t]
plot(x = df.sim.stats$t, y = df.sim.stats$sd, type = 'l')
hist(df.sim[t == 500, x], breaks = 100)
qqnorm(df.sim[t == 500, x])
qqline(df.sim[t == 500, x])

# Random walk model: fits well on data, but is completely wrong
fit.arima <- arima(df$PRESSURE_VALUE, order = c(0, 1, 0))
plot(forecast:::simulate.Arima(fit.arima, 10000))

# See also analysis v18 for a similar study on KNMI data.
# In that case, and if we go down o 1h intervals, then
# there are more AR components needed to simulate it correctly.
# Concretely: there are 2-3 components needed for 1h intervals.
# AR(1) model is too close to a random walk.
