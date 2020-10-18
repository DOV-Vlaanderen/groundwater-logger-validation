# P-value analysis for AR(1) model



# Helper function for correlated errors ----
xy.errors <- function(n = 1000, mu = rep(1032, 2L), Sigma = matrix(c(23, 20, 20, 23), ncol = 2)) {
  if (is.null(sigma)) {
    list('x' = rnorm(n, mean = mu[1], sd = Sigma[1, 1]),
         'y' = rnorm(n, mean = mu[2], sd = Sigma[2, 2]))
  } else {
    as.list(data.frame(
      MASS::mvrnorm(n, mu = c('x' = mu[1], 'y' = mu[2]), Sigma = Sigma)
    ))
  }
}



# AR(1) model: intecept (OK) ----
set.seed(2020)
x <- gwloggeR:::model_drifts.simulate(n = 1000, mu = 1032, sigma = 5, phi1 = 0.9)
y <- gwloggeR:::model_drifts.simulate(n = 1000, mu = 1032, sigma = 5, phi1 = 0.9)
plot(x - y, type = 'l')
forecast::auto.arima(x - y, stationary = TRUE, trace = TRUE)

p <- replicate(n = 1000, expr = {
  n <- 10000
  x <- gwloggeR:::model_drifts.simulate(n = n, mu = 1032, sigma = 5, phi1 = 0.9)
  y <- gwloggeR:::model_drifts.simulate(n = n, mu = 1032, sigma = 5, phi1 = 0.9)
  gwloggeR:::lrtest(
    logLik(arima(x - y, order = c(1, 0, 0), include.mean = FALSE)),
    logLik(arima(x - y, order = c(1, 0, 0))),
    df.diff = 1L
  )
})
hist(p)
sum(p < 0.05)/length(p)



# AR(1) model: drift (NOK) ----
set.seed(2020)
p <- replicate(n = 1000, expr = {
  n <- 1000
  x <- gwloggeR:::model_drifts.simulate(n = n, mu = 1000, sigma = 5, phi1 = 0.9)
  y <- gwloggeR:::model_drifts.simulate(n = n, mu = 1050, sigma = 5, phi1 = 0.9)
  fit <- gwloggeR:::model_drifts.fit(
    x = x - y,
    timestamps = seq(as.POSIXct('2000-01-01'), by = '12 hours', length.out = n),
    ar1 = 0.9, dfdiff = 2
  )
  fit$drift.significance
})
hist(p)
sum(p < 0.05)/length(p)



# Linear model: drift (NOK) ----
set.seed(2020)
list2env(xy.errors(Sigma = diag(c(5, 5))), envir = environment())
plot(x, y)
plot(x - y, type = 'l')

list2env(xy.errors(), envir = environment())
plot(x, y)
plot(x - y, type = 'l')

lin.sig <- function(x, y, dfdiff) {

  stopifnot(length(x) == length(y))
  n <- length(x)

  seekmin <- function(M0, bps, xreg = NULL) {
    for (bp in bps) {
      bptrend <- c(rep(0, bp - 1), trend[bp:length(trend)] - trend[bp])
      reg <- if (!is.null(xreg)) cbind(xreg, bptrend) else cbind(bptrend)
      .M <- lm(x - y ~ reg - 1)
      .M[['xreg']] <- reg
      .M[['bp']] <- bp
      if (logLik(.M) > logLik(M0)) M0 <- .M
    }
    M0
  }

  bps.local <- function(M) {
    unique(c(max(1L, M$bp - 2*sidays):M$bp, M$bp:min(M$bp + 2*sidays, length(x) - 1)))
  }

  # Regressor definitions
  trend <- 1:n
  sidays <- as.integer(sqrt(n)) # first sweep seek interval in days
  bps <- which(!duplicated((trend[-length(trend)] - trend[1L]) %/% (sidays))) # breakpoints for fitting

  M0 <- lm(x - y ~ -1)

  M1 <- seekmin(M0 = M0, bps = bps)
  M1 <- seekmin(M1, bps = bps.local(M1))

  gwloggeR:::lrtest(logLik(M0), logLik(M1), df.diff = dfdiff)
}

p <- replicate(n = 1000, expr = {
  n <- 1000
  list2env(xy.errors(n = n, Sigma = diag(c(5, 5))), envir = environment())
  lin.sig(x = x, y = y, dfdiff = 2.8)
})
hist(p)
sum(p < 0.05)/length(p)



# Linear model: correlated drift (NOK) ----
p <- replicate(n = 1000, expr = {
  n <- 1000
  list2env(xy.errors(n = n), envir = environment())
  lin.sig(x = x, y = y, dfdiff = 2.8)
})
hist(p)
sum(p < 0.05)/length(p)
