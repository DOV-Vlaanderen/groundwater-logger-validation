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

# AR(1) component in case of difference between two barometers -----------------
sim <- function(n, mu, sigma, phi1, betas = NULL, xreg = NULL,
                init = mu, xt = NULL, a = rnorm(n, 0, sd = sigma)) {

  R <- rep(0, n)
  if (!is.null(betas) || !is.null(xreg)) {
    reg <- xreg[-1,,drop=FALSE] - phi1*xreg[-n,,drop=FALSE]
    R <- c(0, reg %*% betas)
  }

  if (is.null(xt)) {
    # xp is the previous x: x_{t-1}
    Reduce(f = function(xp, t) phi1*xp + mu*(1-phi1) + R[t] + a[t], x = 2:n, init = init, accumulate = TRUE)
  } else {
    c(xt[1],
      phi1*xt[-n] + mu*(1-phi1) + R[-1] + a[-1])
  }
}

set.seed(2020)
correlated.errors <- data.table::as.data.table(
  MASS::mvrnorm(1e5, mu = c('a' = 0, 'b' = 0), Sigma = matrix(c(23, 20, 20, 23), ncol = 2))
)
cov(correlated.errors$a, correlated.errors$b)

## all equal: OK (error due to correlation is small --> -2*covariance)
df.diff <- data.table::data.table(
  TIMESTAMP_UTC = seq(from = as.POSIXct('2001-01-01 00:00:00'), by = '12 hours', length.out = 1e5),
  BARO_01 = sim(n = 1e5, phi1 = 0.9, mu = 1032, a = correlated.errors$a),
  BARO_02 = sim(n = 1e5, phi1 = 0.9, mu = 1032, a = correlated.errors$b)
)
df.diff[, DIFF := BARO_01 - BARO_02]
forecast::auto.arima(df.diff$DIFF, trace = TRUE, stationary = TRUE)

## uncorrelated: OK (error = the sum of individual errors)
set.seed(2020)
df.diff <- data.table::data.table(
  TIMESTAMP_UTC = seq(from = as.POSIXct('2001-01-01 00:00:00'), by = '12 hours', length.out = 1e5),
  BARO_01 = sim(n = 1e5, phi1 = 0.9, mu = 1020, sigma = sqrt(23)),
  BARO_02 = sim(n = 1e5, phi1 = 0.9, mu = 1035, sigma = sqrt(23))
)
df.diff[, DIFF := BARO_01 - BARO_02]
forecast::auto.arima(df.diff$DIFF, trace = TRUE, stationary = TRUE)

## mu different: OK (new mu = difference between the two)
df.diff <- data.table::data.table(
  TIMESTAMP_UTC = seq(from = as.POSIXct('2001-01-01 00:00:00'), by = '12 hours', length.out = 1e5),
  BARO_01 = sim(n = 1e5, phi1 = 0.9, mu = 1020, a = correlated.errors$a),
  BARO_02 = sim(n = 1e5, phi1 = 0.9, mu = 1035, a = correlated.errors$b)
)
df.diff[, DIFF := BARO_01 - BARO_02]
forecast::auto.arima(df.diff$DIFF, trace = TRUE, stationary = TRUE)

## sigma different: OK (new sigma: addition of the two minus 2*cov)
set.seed(2020)
correlated.errors.different.sigma <- data.table::as.data.table(
  MASS::mvrnorm(1e5, mu = c('a' = 0, 'b' = 0), Sigma = matrix(c(10, 15, 15, 23), ncol = 2))
)
cov(correlated.errors.different.sigma$a, correlated.errors.different.sigma$b)
df.diff <- data.table::data.table(
  TIMESTAMP_UTC = seq(from = as.POSIXct('2001-01-01 00:00:00'), by = '12 hours', length.out = 1e5),
  BARO_01 = sim(n = 1e5, phi1 = 0.9, mu = 1035, a = correlated.errors.different.sigma$a),
  BARO_02 = sim(n = 1e5, phi1 = 0.9, mu = 1035, a = correlated.errors.different.sigma$b)
)
df.diff[, DIFF := BARO_01 - BARO_02]
forecast::auto.arima(df.diff$DIFF, trace = TRUE, stationary = TRUE)

## phi different: MA component seems necessary, new sigma larger
df.diff <- data.table::data.table(
  TIMESTAMP_UTC = seq(from = as.POSIXct('2001-01-01 00:00:00'), by = '12 hours', length.out = 1e5),
  BARO_01 = sim(n = 1e5, phi1 = 0.5, mu = 1035, a = correlated.errors$a),
  BARO_02 = sim(n = 1e5, phi1 = 0.9, mu = 1035, a = correlated.errors$b)
)
df.diff[, DIFF := BARO_01 - BARO_02]
forecast::auto.arima(df.diff$DIFF, trace = TRUE, stationary = TRUE)

## phi different & uncorrelated: MA component seems necessary, new sigma larger
set.seed(2020)
df.diff <- data.table::data.table(
  TIMESTAMP_UTC = seq(from = as.POSIXct('2001-01-01 00:00:00'), by = '12 hours', length.out = 1e5),
  BARO_01 = sim(n = 1e5, phi1 = 0.5, mu = 1035, sigma = sqrt(23)),
  BARO_02 = sim(n = 1e5, phi1 = 0.9, mu = 1035, sigma = sqrt(23))
)
df.diff[, DIFF := BARO_01 - BARO_02]
forecast::auto.arima(df.diff$DIFF, trace = TRUE, stationary = TRUE)

