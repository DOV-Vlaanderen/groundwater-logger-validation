model_drifts.simulate <- function(n, mu, sigma, phi1, betas = NULL, xreg = NULL,
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

# set.seed(2020)
# x.sim <- model_drifts.simulate(10000, mu = 1032, sigma = sqrt(23), phi1 = 0.9, betas = 0, xreg = matrix(1:10000, ncol = 1))
# ts.sim <- seq(as.POSIXct('2000-01-01'), by = '12 hours', length.out = 10000)
# plot(x = ts.sim, y = x.sim, type = 'l')
# acf(x.sim)
# pacf(x.sim)



model_drifts.fit <- function(x, timestamps, ar1 = 0.90) {

  seekmin <- function(M0, bps, xreg = NULL) {
    for (bp in bps) {
      bptrend <- c(rep(0, bp - 1), trend[bp:length(trend)] - trend[bp])
      reg <- if (!is.null(xreg)) cbind(xreg, bptrend) else cbind(bptrend)
      .M <- arima(x = x, order = c(1, 0, 0), xreg = reg, transform.pars = FALSE, fixed = c(ar1, rep(NA, 1L + ncol(reg))))
      .M[['xreg']] <- reg
      .M[['bp']] <- bp
      .M[['bp.ts']] <- ts.adj[bp]
      if (logLik(.M) > logLik(M0)) M0 <- .M
    }
    M0
  }

  bps.local <- function(M) {
    unique(c(max(1L, M$bp - 2*sidays):M$bp, MD$bp:min(M$bp + 2*sidays, length(x.adj) - 1)))
  }

  # Sanity checks
  assert.timestamp(timestamps)
  assert.nonas(timestamps)
  assert.ordered(timestamps)
  stopifnot(length(x) == length(timestamps))
  stopifnot(!any(is.na(x)))

  # Aggregating x to minimum 12h intervals: x.adj and ts.adj
  list2env(
    aggregate(
      list('x.adj' = x),
      by = list('ts.adj' = round.timestamp(timestamps, scalefactor.sec = 3600*12)),
      FUN = mean),
    envir = environment()
  )

  if (length(x) != length(x.adj))
    warning('Timeseries has intervals smaller than 12h. ' %||%
            'Measurements are averaged to 12h intervals.', call. = FALSE, immediate. = TRUE)

  stopifnot(length(x.adj) >= 5L) # TODO: should return no drift

  ts.sec <- as.numeric(ts.adj)
  ts.sec.diff <- diff(ts.sec)

  stopifnot(all(ts.sec.diff >= 12*3600))

  if (sum(ts.sec.diff > 12*3600)/length(x.adj) > 0.01)
    warning('More than 1 % of time intervals is larger than 12h. ' %||%
            'The current drift detection model works best for 12h interval measurements.',
            call. = FALSE, immediate. = TRUE)

  # Regressor definitions
  trend <- c(0, cumsum(ts.sec.diff/3600/24/365.25))
  sidays <- 15 # first sweep seek interval in days
  bps <- which(!duplicated((ts.sec[-length(ts.sec)] - ts.sec[1L]) %/% (sidays*24*3600))) # breakpoints for fitting
  sbasis <- fbasis(timestamps = timestamps, frequencies = 1/(365.25*3600*24))

  # Models
  M0 <- arima(x = x.adj, order = c(1, 0, 0), transform.pars = FALSE, fixed = c(ar1, NA))

  MD <- seekmin(M0, bps = bps)
  MD <- seekmin(MD, bps = bps.local(MD))

  MDS <- seekmin(MD, bps = c(MD$bp, bps), xreg = sbasis)
  MDS <- seekmin(MDS, bps = bps.local(MDS), xreg = sbasis)

  # Model selection
  sp.val <- lrtest(logLik(MD), logLik(MDS), df.diff = 2L) # p-val for seasonality component
  MF <- if (sp.val < 1e-2) MDS else MD # final model

  MF
}

# model_drifts.fit(x = x.sim, timestamps = ts.sim)
