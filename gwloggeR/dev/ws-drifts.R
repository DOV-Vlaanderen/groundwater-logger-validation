# Workspace for development of detect_drift() in airpressure series.
# ------------------------------------------------------------------------------

xy.errors <- function(n, mu = rep(1032, 2L), Sigma = matrix(c(23, 20, 20, 23), ncol = 2)) {
  as.list(data.frame(
    MASS::mvrnorm(n, mu = c('x' = mu[1], 'y' = mu[2]), Sigma = Sigma)
  ))
}

ts <- seq(as.POSIXct('2010-01-01'), by = '12 hours', length.out = 5000)
sb <- data.matrix(fbasis(timestamps = ts, frequencies = 1/(365.25*3600*24)))
tr <- cumsum(c(0, diff(as.numeric(ts))))/365.25/24/3600

set.seed(2020)
list2env(xy.errors(n = length(ts), mu = c(0,0)), envir = environment())

gwloggeR::detect_drift(
  x = gwloggeR:::model_drifts.simulate(mu = 1032, phi1 = 0.9, a = x, betas = c(-0.5, 0.35, 1.5), xreg = cbind(tr, sb)),
  timestamps = ts, verbose = TRUE, plot = TRUE, alpha = 1,
  reference = list(
    list(x = gwloggeR:::model_drifts.simulate(mu = 1032, phi1 = 0.9, a = y),
         timestamps = ts)
  )
)

# ------------------------------------------------------------------------------

options(logger.root.data.path = './../data/raw')
try(devtools::load_all('./../gwloggeR.data', export_all = FALSE))
try(detach('package:gwloggeR.data'))

# df <- gwloggeR.data::read('BAOL540X_B_00612')$df # has an outlier at the end due to big timediff
# df <- gwloggeR.data::read('BAOL008X_72528')$df
# df <- gwloggeR.data::read('BAOL087X_179808')$df # has a drift at the end: 2 outlying points
# df <- gwloggeR.data::read('BAOL110X_637648')$df # wrongly detected outlier at the end due to 2 outlying points
# df <- gwloggeR.data::read('BAOL093')$df # MAD best detection due to outliers at the end
# df <- gwloggeR.data::read('BAOL005X_B5')$df # very pronounced long, but weak drift
# df <- gwloggeR.data::read('BAOL545X_B_006DC')$df # right side missing
# df <- gwloggeR.data::read('BAOL900X')$df # left side missing
# df <- gwloggeR.data::read('BAOL077X')$df # outlier at the end
# df <- gwloggeR.data::read('BAOL824X_P2_15563')$df # wrongly detected outlier at the end
# df <- gwloggeR.data::read('BAOL822X')$df # seasonality not detected
# df <- gwloggeR.data::read('BAOL528X_B_B2152')$df # 3 points
df <- gwloggeR.data::read('BAOL070X_B9393')$df

df <- df[!is.na(PRESSURE_VALUE),]
df <- df[!is.na(TIMESTAMP_UTC),]
data.table::setkey(df, TIMESTAMP_UTC)

# Single reference
# df.ref <- gwloggeR.data::read('Westdorpe')$df
# ref <- list(list(x = df.ref[!is.na(PRESSURE_VALUE), PRESSURE_VALUE], timestamps = df.ref[!is.na(PRESSURE_VALUE), TIMESTAMP_UTC]))

# Multi reference
ref <- sapply(
  tools::file_path_sans_ext(grep('20201103', gwloggeR.data::enumerate(partner = 'knmi'), value = TRUE)),
  function(name) {
    df.ref <- gwloggeR.data::read(name)$df
    list('x' = df.ref[!is.na(PRESSURE_VALUE), PRESSURE_VALUE],
         'timestamps' = df.ref[!is.na(PRESSURE_VALUE), TIMESTAMP_UTC])
  }, simplify = FALSE, USE.NAMES = TRUE
)


gwloggeR::detect_drift(
  x = df$PRESSURE_VALUE,
  timestamps = df$TIMESTAMP_UTC,
  verbose = TRUE,
  plot = TRUE,
  reference = ref,
  alpha = 1
)

