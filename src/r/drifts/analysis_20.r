# Seasonality analysis

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

df.list <- sapply(basename(logger.names), read.baro, simplify = FALSE, USE.NAMES = TRUE)
# df <- data.table::rbindlist(df.list, use.names = TRUE, fill = TRUE)
# data.table::setkey(df, FILE, TIMESTAMP_UTC)
# df

data.ref <- read.baro('KNMI_20200312_hourly')
with(data.ref, plot(x = TIMESTAMP_UTC, y = PRESSURE_VALUE, type = 'l'))

compare <- function(df, df.ref) {
  if (nrow(df) == 0L || nrow(df.ref) == 0L) return(data.table::data.table())
  df <- data.table::copy(df)
  df.ref <- data.table::copy(df.ref)

  # Center based on median
  df[, 'PRESSURE_VALUE' := PRESSURE_VALUE - median(PRESSURE_VALUE)]
  df.ref[, 'PRESSURE_VALUE' := PRESSURE_VALUE - median(PRESSURE_VALUE)]

  df.diff <- df[J(df.ref), .(
    'PRESSURE_VALUE' = x.PRESSURE_VALUE,
    'PRESSURE_VALUE_REF' = i.PRESSURE_VALUE,
    'PRESSURE_DIFF' = x.PRESSURE_VALUE - i.PRESSURE_VALUE,
    TIMESTAMP_UTC)
    ][!is.na(PRESSURE_DIFF), ]

  if (nrow(df.diff) < 100L) return(data.table::data.table())

  structure(df.diff, 'logger.name' = attr(df, 'logger.name'))
}

df.diff <- compare(df.list[["BAOL828X_P2_15705.csv"]], df.ref = data.ref)
plot(df.diff$PRESSURE_DIFF, type = 'l', x = df.diff$TIMESTAMP_UTC)

source('./../../gwloggeR/R/fourier.R')

fbase <- fbasis(df.diff$TIMESTAMP_UTC, trend = TRUE,
                frequencies = 1/seq(from = 12*3600, to = diff(as.numeric(range(df.diff$TIMESTAMP_UTC))), length.out = 10000))

#cl <- parallel::makeCluster(5L)
#doParallel::registerDoParallel(cl)
#parallel::stopCluster(cl)

fit <- glmnet::cv.glmnet(df.diff$PRESSURE_DIFF,
                         x = data.matrix(fbase),
                         alpha = 1,
                         #parallel = TRUE,
                         nlambda = 500L,
                         foldid = rep(1:10, each = nrow(fbase)/10, length.out = nrow(fbase)))
fit
plot(fit)

lambda.se <- fit$lambda.1se - fit$lambda.min

plot(glmnet::predict.glmnet(fit$glmnet.fit, newx = data.matrix(fbase), s = fit$lambda.1se), type = 'l', x = df.diff$TIMESTAMP_UTC)

coefs <- function(fit, lambda) {
  coef <- glmnet::coef.glmnet(fit$glmnet.fit, s = lambda)
  coef[coef[, '1'] != 0,,drop = FALSE]
}
coefs(fit, lambda = fit$lambda.1se)

# DTFT analysis

dtft(frequency = 1/(365.25*24*3600), x = df.diff$PRESSURE_DIFF, timestamps = df.diff$TIMESTAMP_UTC)
dtft(frequency = 4/diff(as.numeric(range(df.diff$TIMESTAMP_UTC))), x = df.diff$PRESSURE_DIFF, timestamps = df.diff$TIMESTAMP_UTC)

period <- seq(from = 12*3600*4, to = diff(as.numeric(range(df.diff$TIMESTAMP_UTC))), length.out = 10000)
fd <- sapply(1/period, FUN = dtft,
             x = df.diff$PRESSURE_DIFF, timestamps = df.diff$TIMESTAMP_UTC)
plot(Mod(fd), type = 'l', x = period/3600/24)
period[which.max(Mod(fd))]/3600/24
table(diff(df.diff$TIMESTAMP_UTC))

# FFT analysis

z <- fft(df.diff$PRESSURE_DIFF)
P <- diff(as.numeric(range(df.diff$TIMESTAMP_UTC)))
P1 <- P/(length(z) - 1)
fft.periods <- 1/((0:(length(z) - 1))*P1/P)
plot(Mod(z), type = 'l',
     x = fft.periods,
     xlab = 'days')
fft.periods[order(Mod(z), decreasing = TRUE)][1:5]
spectrum(df.diff$PRESSURE_DIFF)
dft(1L, df.diff$PRESSURE_DIFF)

