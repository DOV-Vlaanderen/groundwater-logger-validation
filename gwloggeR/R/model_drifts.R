model_drifts.simulate <- function(n = length(a), mu, sigma, phi1, betas = NULL, xreg = NULL,
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

model_drifts.trend <- function(timestamps, start.ts) {
  trending.ts <- timestamps[timestamps >= start.ts]
  c(rep(0, length(timestamps) - length(trending.ts) + 1L),
    cumsum(diff(as.numeric(trending.ts)))/3600/24/365.25)
}

#' Fits a drift detection model
#'
#' @return Arima or ArimaExt object
#'
#' @keywords internal
#'
model_drifts.fit <- function(dr.x, dr.ts, ar1, dfdiff) {

  seekmin <- function(M0, x, ts, idx = NULL, xreg = NULL) {

    si <- ceiling(sqrt(length(x)/2)) # n=x*2x (2x due to left + right lookup)
    if (is.null(idx)) {
      # length(x) - si: trick to not look to much at the end
      bps <- c(seq(from = 1L, to = length(x) - si - 1, by = si), length(x) - si)
    } else bps <- idx

    for (bp in bps) {
      .bptrend <- model_drifts.trend(ts, ts[bp])
      .xreg <- if (!is.null(xreg)) cbind(xreg, 'bptrend' = .bptrend) else cbind('bptrend' = .bptrend)
      .M <- arima(x = x, order = c(1, 0, 0), xreg = .xreg, transform.pars = FALSE, fixed = c(ar1, rep(NA, 1L + ncol(.xreg))))
      .M[['xreg']] <- .xreg
      .M[['bp']] <- bp
      .M[['bp.ts']] <- ts[bp]
      if (logLik(.M) > logLik(M0)) M0 <- .M
    }

    if (is.null(idx))
      M0 <- seekmin(
        M0 = M0, x = x, ts = ts, xreg = xreg,
        idx = unique(c(max(1L, M0$bp - si):M0$bp,
                       M0$bp:min(M0$bp + si, length(x) - 1)))
      )

    M0
  }

  # Sanity checks
  assert.timestamp(dr.ts)
  assert.nonas(dr.ts)
  assert.ordered(dr.ts)
  stopifnot(length(dr.x) == length(dr.ts))
  stopifnot(!any(is.na(dr.x)))
  stopifnot(length(dr.x) >= 2L) # need at least 2 observations for arima()

  ts.sec <- as.numeric(dr.ts)
  ts.sec.diff <- diff(ts.sec)

  stopifnot(all(ts.sec.diff >= 12*3600))

  if (sum(ts.sec.diff > 12*3600)/length(dr.x) > 0.01)
    warning('More than 1 % of time intervals is larger than 12h. ' %||%
            'The current drift detection model works best for 12h interval measurements.',
            call. = FALSE, immediate. = TRUE)

  # Regressor definitions
  sbasis <- fbasis(timestamps = dr.ts, frequencies = 1/(365.25*3600*24))

  # Models
  M <- arima(x = dr.x, order = c(1, 0, 0), transform.pars = FALSE, fixed = c(ar1, NA))

  # Filter out the worst case outliers so they do not result in detected drifts.
  # This has mainly an effect for drifts towards the end of the series, cased
  # by strong outliers.
  # It might well be that there will still remain outliers because their
  # outlying effect was suppressed by the now removed outlier (due to AR1 effect).
  ol <- detect_outliers(as.vector(residuals(M)))

  M0 <- arima(x = dr.x[!ol], order = c(1, 0, 0), transform.pars = FALSE, fixed = c(ar1, NA))

  if (length(dr.x) < 5L) {
    warning('Can not detect drift because differences with reference data have less than 5 observations. ' %||%
            'Returning nu drift by default.',
            call. = FALSE, immediate. = TRUE)
    return(M0)
  }

  MS <- arima(x = dr.x[!ol], order = c(1, 0, 0), transform.pars = FALSE,
              xreg = sbasis[!ol,,drop=FALSE], fixed = c(ar1, NA, NA, NA))

  MD <- seekmin(M0, x = dr.x[!ol], ts = dr.ts[!ol])

  MDS <- seekmin(MS, x = dr.x[!ol], ts = dr.ts[!ol], xreg = sbasis[!ol,,drop=FALSE])

  # Model selection
  # p-val for seasonality component: eventually, only either sin or cos
  # are needed in case fase is 0, so df == 1. This also produces slightly
  # better results.
  sp.val <- lrtest(logLik(MD), logLik(MDS), df.diff = 1L)
  MF <- if (sp.val < 1e-2) MDS else MD # final model

  MND <- if (any(grepl('sin', names(MF$coef)))) MS else M0
  MF[['MND']] <- MND

  # Significance of drift component
  MF[['drift.significance']] <-
    lrtest(logLik(MND), logLik(MF), df.diff = dfdiff)

  structure(MF, class = c('ArimaExt', class(MF)))
}

# model_drifts.fit(x = x.sim, timestamps = ts.sim)
