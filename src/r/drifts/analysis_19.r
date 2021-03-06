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

  # Normalize barometer so the mu is the same for every barometer
  # df[, 'PRESSURE_VALUE' := PRESSURE_VALUE - median(PRESSURE_VALUE)]

  data.table::setkey(df, TIMESTAMP_UTC)
  structure(df, 'logger.name' = logger.name)
}

df.list <- lapply(logger.names, read.baro)
df <- data.table::rbindlist(df.list, use.names = TRUE, fill = TRUE)
data.table::setkey(df, FILE, TIMESTAMP_UTC)
df
ggplot2::ggplot(data = df, ggplot2::aes(x = TIMESTAMP_UTC, y = PRESSURE_VALUE, color = FILE)) +
  ggplot2::geom_line(show.legend = FALSE)

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
# Coefficient is 0.90 with sigma2 = 23.62
fit.arima <- arima(df$PRESSURE_VALUE, order = c(1, 0, 0))
plot(forecast:::simulate.Arima(fit.arima, 10000))

# Distribution of AR(1) model (coef = 0.90)
# Seems to be normal given a timestamp.
# So according to this approximation, a height-compensated barometer should be
# within a normal distribution with mu = 1033 and sd = 11.137
# Note that assumption is that the barometers are height-compensated.
# With coef = 0.82 the sd = 9.9
df.sim <- data.table::rbindlist(lapply(1:1000, function(x) data.frame(t = 1:1000, x = forecast:::simulate.Arima(fit.arima, 1000))))
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

# Verified barometers
dfv <- data.table::copy(gwloggeR:::airpressure)
dfv <- dfv[, .('PRESSURE_VALUE' = mean(PRESSURE_VALUE)),
           by = .('TIMESTAMP_UTC' = round_timestamp(TIMESTAMP_UTC), FILE)]
data.table::setkey(dfv, FILE, TIMESTAMP_UTC)
ggplot2::ggplot(data = dfv, ggplot2::aes(x = TIMESTAMP_UTC, y = PRESSURE_VALUE, color = FILE)) +
  ggplot2::geom_line(show.legend = FALSE)
arima(dfv$PRESSURE_VALUE, order = c(1, 0, 0))
# Here also the most impact has the AR(1) model, after that the impact is not large
# The AR(1) coefficient is here 0.82 at 12h intervals with sigma2 = 31.85

# 65% non-drinfting barometers
analysis_10 <- data.table::fread(file = './drifts/analysis_10/baro_median_errors.csv')
analysis_10 <- analysis_10[order(WISK, decreasing = TRUE),]
dfw <- data.table::rbindlist(lapply(analysis_10[WISK <= quantile(WISK, probs = 0.65), FILE], read.baro), use.names = TRUE, fill = TRUE)
data.table::setkey(dfw, FILE, TIMESTAMP_UTC)
dfw
ggplot2::ggplot(data = dfw, ggplot2::aes(x = TIMESTAMP_UTC, y = PRESSURE_VALUE, color = FILE)) +
  ggplot2::geom_line(show.legend = FALSE)
arima(dfw$PRESSURE_VALUE, order = c(1, 0, 0))
# Here the most impact has the AR(1) model, after that the impact is not large
# The AR(1) coefficient is here 0.89 at 12h intervals with sigma2 = 21.3!

# Normalizing the series around the median has little effect on AR(1) coef:
# it remains around 0.88, and sigma2 also remains around 21-23.
# This was tested on both df and dfw.
# So we can conclude that for modeling air pressure AR(1) is a good model.
