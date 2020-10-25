# P-value analysis for AR(1) model
# We are mainly interested here in the distribution of p-values
# in case of drift an LR test. The key question is whether the distribution
# is uniform.



# Helper function for correlated errors ----
xy.errors <- function(n = 1000, mu = rep(1032, 2L), Sigma = matrix(c(23, 20, 20, 23), ncol = 2)) {
  as.list(data.frame(
    MASS::mvrnorm(n, mu = c('x' = mu[1], 'y' = mu[2]), Sigma = Sigma)
  ))
}

lin.sig <- function(z, dfdiff) {

  n <- length(z)

  seekmin <- function(M0, bps, xreg = NULL) {
    for (bp in bps) {
      bptrend <- c(rep(0, bp - 1), trend[bp:length(trend)] - trend[bp])
      reg <- if (!is.null(xreg)) cbind(xreg, bptrend) else cbind(bptrend)
      .M <- lm(z ~ reg - 1)
      .M[['xreg']] <- reg
      .M[['bp']] <- bp
      if (logLik(.M) > logLik(M0)) M0 <- .M
    }
    M0
  }

  bps.local <- function(M) {
    unique(c(max(1L, M$bp - 2*sidays):M$bp, M$bp:min(M$bp + 2*sidays, length(z) - 1)))
  }

  # Regressor definitions
  trend <- 1:n
  sidays <- as.integer(sqrt(n)) # first sweep seek interval in days
  bps <- which(!duplicated((trend[-length(trend)] - trend[1L]) %/% (sidays))) # breakpoints for fitting

  M0 <- lm(z ~ -1)

  M1 <- seekmin(M0 = M0, bps = bps)
  M1 <- seekmin(M1, bps = bps.local(M1))

  gwloggeR:::lrtest(logLik(M0), logLik(M1), df.diff = dfdiff)
}



# AR(1) model: intecept (OK) ----
set.seed(2020)
list2env(xy.errors(n = 1000, mu = c(0,0), Sigma = diag(c(5, 5))), envir = environment())
x <- gwloggeR:::model_drifts.simulate(mu = 1032, phi1 = 0.9, a = x)
y <- gwloggeR:::model_drifts.simulate(mu = 1032, phi1 = 0.9, a = y)
plot(x - y, type = 'l')
forecast::auto.arima(x - y, stationary = TRUE, trace = TRUE)

p <- replicate(n = 1000, expr = {
  n <- 10000
  list2env(xy.errors(n = n, mu = c(0,0), Sigma = diag(c(5, 5))), envir = environment())
  x <- gwloggeR:::model_drifts.simulate(mu = 1032, phi1 = 0.9, a = x)
  y <- gwloggeR:::model_drifts.simulate(mu = 1032, phi1 = 0.9, a = y)
  gwloggeR:::lrtest(
    logLik(arima(x - y, order = c(1, 0, 0), include.mean = FALSE)),
    logLik(arima(x - y, order = c(1, 0, 0))),
    df.diff = 1L
  )
})
hist(p)
sum(p < 0.05)/length(p)
# pvals are distributed uniformly



# AR(1) model: drift (NOK) ----
set.seed(2020)
p <- replicate(n = 1000, expr = {
  n <- 1000
  list2env(xy.errors(n = n, mu = c(0,0), Sigma = diag(c(5, 5))), envir = environment())
  x <- gwloggeR:::model_drifts.simulate(mu = 1000, phi1 = 0.9, a = x)
  y <- gwloggeR:::model_drifts.simulate(mu = 1050, phi1 = 0.9, a = y)
  fit <- gwloggeR:::model_drifts.fit(
    x = x - y,
    timestamps = seq(as.POSIXct('2000-01-01'), by = '12 hours', length.out = n),
    ar1 = 0.9,
    dfdiff = 2.8
  )
  fit$drift.significance
})
hist(p)
sum(p < 0.05)/length(p)
# pvals are not distributed uniformly under 2L df difference LR test.
# LR test df difference seems also to be dependent on number of observations (n).
# For 1000-10000 obs. a df difference of 2.8 seems to produce good results.



# AR(1) model: correlated + drift (NOK) ----
set.seed(2020)
p <- replicate(n = 1000, expr = {
  n <- 1000
  list2env(xy.errors(n = n, mu = c(0,0)), envir = environment())
  x <- gwloggeR:::model_drifts.simulate(mu = 1000, phi1 = 0.9, a = x)
  y <- gwloggeR:::model_drifts.simulate(mu = 1050, phi1 = 0.9, a = y)
  fit <- gwloggeR:::model_drifts.fit(
    x = x - y,
    timestamps = seq(as.POSIXct('2000-01-01'), by = '12 hours', length.out = n),
    ar1 = 0.9,
    dfdiff = 2.8
  )
  fit$drift.significance
})
hist(p)
sum(p < 0.05)/length(p)
# Doesn't seem to be different from non-correlated AR(1) drift.



# Linear model: drift (NOK) ----
set.seed(2020)
list2env(xy.errors(Sigma = diag(c(5, 5))), envir = environment())
plot(x, y)
plot(x - y, type = 'l')

list2env(xy.errors(), envir = environment())
plot(x, y)
plot(x - y, type = 'l')

p <- replicate(n = 1000, expr = {
  n <- 1000
  list2env(xy.errors(n = n, Sigma = diag(c(5, 5))), envir = environment())
  lin.sig(z = x - y, dfdiff = 2.8)
})
hist(p)
sum(p < 0.05)/length(p)
# pvals are not distributed uniformly under 2L df difference LR test.
# LR test df difference seems also to be dependent on number of observations (n).
# For 1000-10000 obs. a df difference of 2.8 seems to produce good results, just like in AR(1) case.



# Linear model: correlated + drift (NOK) ----
p <- replicate(n = 1000, expr = {
  n <- 1000
  list2env(xy.errors(n = n), envir = environment())
  lin.sig(z = x - y, dfdiff = 2.8)
})
hist(p)
sum(p < 0.05)/length(p)
# Doesn't seem to be different from non-correlated drift.
