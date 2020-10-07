# Likelihood and optimization of an AR(1) model with trend
# Rest is taken over from analysis v23

# Model ------------------------------------------------------------------------

set.seed(2020)

#' LogLikelihood of AR(1) model with drift and seasonality
#'
#' Box & Jenkins suggest using unconditional likelihood, but here I have taken
#' the approach of excluding p + q variables for ease.
#'
#' @param mu,sigma,phi scalars
#' @param betas vector of betas for xreg
#' @param xreg matrix of regressors similar to mu
#'
#' @keywords internal
logL <- function(x, mu, sigma, phi1, betas = NULL, xreg = NULL) {
  n <- length(x)
  M <- phi1*x[-n] + mu * (1 - phi1)
  if (!is.null(betas) || !is.null(xreg)) {
    reg <- xreg[-1,,drop=FALSE] - phi1*xreg[-n,,drop=FALSE]
    M <- M + reg %*% betas
  }
  squared <- (x[-1] - M)^2
  sum(-squared/(2*sigma^2)) - (n - 1)*log(sigma * sqrt(2*pi))
}

logL(rnorm(100), mu = 0, sigma = 1, phi1 = 5, betas = 2, xreg = matrix(c(rep(0, 4), 1:(100-4))))
logL(rnorm(100), mu = 0, sigma = 1, phi1 = 5)

logL.compile <- function(mu = NULL, sigma = NULL, phi1 = NULL, betas = list()) {

  stopifnot(length(betas) == 0 || !is.null(names(betas)))

  i <- 0

  fn.str <- sprintf(
    'function(par, x, xreg = NULL) {
      logL(x = x, mu = %s, sigma = %s, phi1 = %s, betas = %s, xreg = xreg)
    }'
    , if (is.null(mu)) sprintf('par[%i]', (i <- i + 1)) else mu
    , if (is.null(sigma)) sprintf('par[%i]', (i <- i + 1)) else sigma
    , if (is.null(phi1)) sprintf('par[%i]', (i <- i + 1)) else phi1
    , if (length(betas) > 0L)
      sprintf('c(%s)', paste(names(betas), '=', vapply(betas, function(v)
        if (is.null(v)) sprintf('par[%i]', (i <<- i + 1)) else as.character(v), ''), collapse = ', '))
      else 'NULL'
  )

  fn <- eval(parse(text = fn.str))
  environment(fn) <- globalenv()

  fn
}

(logL.fn <- logL.compile(phi1 = 0, betas = list('b1' = 1, 'b2' = NULL, 'b3' = 3)))
logL.fn(par = c(1,2,3), x = rnorm(100), xreg = matrix(rnorm(300), ncol = 3))
(logL.fn <- logL.compile(phi1 = 0))
logL.fn(par = c(1,2), x = rnorm(100))


sim <- function(n, mu, sigma, phi1, betas = NULL, xreg = NULL,
                init = mu, xt = NULL, a = rnorm(n, 0, sd = sigma)) {

  R <- 0
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

x.sim <- sim(10000, mu = 1032, sigma = sqrt(23), phi1 = 0.9, betas = 0, xreg = matrix(1:10000, ncol = 1))
plot(x.sim, type = 'l')
acf(x.sim)
pacf(x.sim)


x.sim.trend <- sim(10000, mu = 1032, sigma = sqrt(23), phi1 = 0.9, betas = 0.005, xreg = matrix(1:10000, ncol = 1))
plot(x.sim.trend, type = 'l')


#' Fit the custom drift detection model
#'
#' If specific parameters are supplied, then they will be fixed.
#'
#' @keywords internal
fit <- function(x, mu = NULL, sigma = NULL, phi1 = NULL,
                betas = if (is.null(xreg)) list() else setNames(vector('list', ncol(xreg)), colnames(xreg)),
                xreg = NULL) {

  stopifnot(is.null(xreg) || is.matrix(xreg))
  stopifnot(!is.matrix(xreg) || length(betas) == ncol(xreg))
  stopifnot(!is.matrix(xreg) || all(names(betas) == colnames(xreg)))

  all.param.names <- c('mu', 'sigma', 'phi1', names(betas))
  stopifnot(length(unique(all.param.names)) == length(all.param.names))

  stopifnot(!any(ls() %in% names(betas)))
  list2env(betas, envir = environment())

  inits <- c(
    'mu'= median(x),
    'sigma' = var(x),
    'phi1' = pacf(x, plot = FALSE, lag.max = 1)$acf[1,1,1],
    if (length(betas) == 0L) NULL else setNames(coef(lm(x ~ xreg))[-1], names(betas))
  )

  scale <- c(
    'mu' = if (length(betas) == 0L) 1 else summary(lm(x ~ xreg))$coefficients[1L, 2L],
    'sigma' = 1,
    'phi1' = 1/100,
    if (length(betas) == 0L) NULL else setNames(summary(lm(x ~ xreg))$coefficients[-1L, 2L], names(betas))
  )

  lower.bound <- c(
    'mu' = -Inf,
    'sigma' = 1e-7,
    'phi1' = -Inf,
    setNames(rep(-Inf, length(betas)), names(betas))
  )

  upper.bound <- c(
    'mu' = Inf,
    'sigma' = Inf,
    'phi1' = 1 - 1e-7,
    setNames(rep(Inf, length(betas)), names(betas))
  )

  fixed <- unlist(mget(all.param.names)[!sapply(mget(all.param.names), is.null)])
  param.names <- setdiff(all.param.names, names(fixed))

  opt <- optim(
    par = inits[param.names],
    fn = logL.compile(mu = mu, sigma = sigma, phi1 = phi1, betas = betas),
    method = 'L-BFGS-B',
    x = x,
    lower = lower.bound[param.names],
    upper = upper.bound[param.names],
    xreg = xreg,
    control = list('fnscale' = -1,
                   'parscale' = scale[param.names],
                   'trace' = 0)
  )

  # Add fixed parameters
  opt <- append(x = opt, values = list('par.fixed' = fixed), after = 1)

  opt
}

xreg.trend.1 <- matrix(data = 1:length(x.sim), dimnames = list(NULL, 'trend'))

# Here specification of likelihood is tested with general arima model.
## x.sim
fit(x = x.sim) # -29832.06
fit(x = x.sim, xreg = xreg.trend.1)
fit(x = x.sim, betas = list('trend' = 0), xreg = xreg.trend.1) # -29832.06

#forecast::auto.arima(y = x.sim, trace = TRUE)
fit.arima <- arima(x = x.sim, order = c(1, 0, 0))
fit.arima # -29835.35

## x.sim.trend
fit(x = x.sim.trend, xreg = xreg.trend.1) # -29997.49
fit.arima <- arima(x = x.sim.trend, order = c(1, 0, 0), xreg = xreg.trend.1)
fit.arima # -30000.82

# Data -------------------------------------------------------------------------

source('./../../gwloggeR/R/fourier.R')

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

ref.compare <- function(logger.name) {
  compare(read.baro(logger.name), read.baro('KNMI_20200312_hourly'))
}

coef.pvals <- function(M) {
  (1-pnorm(abs(M$coef)/sqrt(diag(M$var.coef))))*2
}

plt.comp <- function(TIMESTAMP_UTC, Y, M = NULL, significant = NULL, front_layer = NULL,
                     xlim = range(TIMESTAMP_UTC), ylim = quantile(Y, c(0.005, 0.995))) {

  force(TIMESTAMP_UTC); force(Y); force(M)
  force(ylim); force(xlim)

  p <- ggplot2::ggplot(mapping = ggplot2::aes(x = TIMESTAMP_UTC, y = Y)) +
    front_layer +
    ggplot2::geom_line(col = 'red', alpha = 0.9)

  if (!is.null(M))
    p <- p + ggplot2::geom_line(mapping = ggplot2::aes(y = fitted(M)), col = 'black', size = 1.5)

  if (!is.null(significant)) {
    p <- p + ggplot2::annotate(
      "label", x = as.POSIXct(-Inf, origin = '1970-01-01'), y = Inf,
      size = 4, col = if (significant) 'green' else 'red',
      hjust = 0, vjust = 1, fill = 'grey', label.size = NA,
      label = if (significant) 'SIGNIFINCANT' else 'NOT SIGNIFICANT'
    )
  }

  if (!is.null(M[['btrend']]) && coef.pvals(M)['btrend'] < 0.001)
    p <- p + ggplot2::geom_vline(xintercept = TIMESTAMP_UTC[which.max(M$btrend > 0)],
                                 col = 'blue', linetype = 'dotted', size = 1.2)

  p <- p +
    ggplot2::coord_cartesian(ylim = ylim, xlim = xlim) +
    ggplot2::ylab('PRESSURE_DIFF') +
    ggplot2::theme_light() +
    ggplot2::theme(axis.text.y = ggplot2::element_text(angle = 90, hjust = 0.5))

  p
}

report <- function(logger.name) {

  environment(plt.comp) <- environment() # fitted(Arima) then can find the source data.

  print(logger.name)

  df.diff <- ref.compare(logger.name)

  if (nrow(df.diff) < 10L) return(invisible(FALSE))

  # M <- fit(x = df.diff$PRESSURE_DIFF, phi1 = 0, d = 0, dsi = 1)
  M <- arima(x = df.diff$PRESSURE_DIFF, order = c(0, 0, 0))
  df.diff[, M_pred := fitted(M)]

  # M.AR <- fit(x = df.diff$PRESSURE_DIFF, d = 0, dsi = 1)
  M.AR <- arima(x = df.diff$PRESSURE_DIFF, order = c(1, 0, 0))
  df.diff[, M.AR.pred := fitted(M.AR)]

  trend <- c(0, cumsum(diff(as.numeric(df.diff$TIMESTAMP_UTC))/3600/24))
  breakpoints <- seq(from = 1, to = nrow(df.diff), by = 10)
  # M.ARD <- M.AR
  # for (bp in breakpoints) {
  #   .M <- fit(x = df.diff$PRESSURE_DIFF, dsi = bp)
  #   if (.M$value > M.ARD$value) M.ARD <- .M
  # }
  M.ARD <- M.AR
  for (bp in breakpoints) {
    btrend <- c(rep(0, bp), trend[-(1:bp)] - bp/2)
    .M <- arima(x = df.diff$PRESSURE_DIFF, order = c(1, 0, 0), xreg = btrend)
    .M[['btrend']] <- btrend
    if (logLik(.M) > logLik(M.ARD)) M.ARD <- .M
  }
  df.diff[, M.ARD.pred := fitted(M.ARD)]


  sbasis <- fbasis(timestamps = df.diff$TIMESTAMP_UTC, frequencies = 1/(365.25*3600*24))

  M.ARS <- arima(x = df.diff$PRESSURE_DIFF, order = c(1, 0, 0), xreg = sbasis)

  M.ARDS <- M.ARS
  for (bp in breakpoints) {
    btrend <- c(rep(0, bp), trend[-(1:bp)] - bp/2)
    .M <- arima(x = df.diff$PRESSURE_DIFF, order = c(1, 0, 0), xreg = cbind(sbasis, btrend))
    .M[['btrend']] <- btrend
    if (logLik(.M) > logLik(M.ARDS)) M.ARDS <- .M
  }

  p.comp <- plt.comp(df.diff$TIMESTAMP_UTC, df.diff$PRESSURE_DIFF)

  p.M <- plt.comp(df.diff$TIMESTAMP_UTC, df.diff$PRESSURE_DIFF, M)

  p.M.AR <- plt.comp(df.diff$TIMESTAMP_UTC, df.diff$PRESSURE_DIFF, M.AR)

  p.M.ARS <- plt.comp(df.diff$TIMESTAMP_UTC, df.diff$PRESSURE_DIFF, M.ARS)

  p.M.ARD <- plt.comp(df.diff$TIMESTAMP_UTC, df.diff$PRESSURE_DIFF, M.ARD)

  p.M.ARDS <- plt.comp(df.diff$TIMESTAMP_UTC, df.diff$PRESSURE_DIFF, M.ARDS)

  # File export ----------------------------------------------------------------

  filename <- sprintf('./drifts/analysis_25/%s.png', tools::file_path_sans_ext(basename(logger.name)))
  dir.create(dirname(filename), showWarnings = FALSE, recursive = TRUE)

  layout_matrix <- rbind(c(1, 2),
                         c(3, 4),
                         c(5, 6))

  grob.title <- grid::textGrob(sprintf('%s (#%s differences with KNMI data from %s to %s)',
                                       basename(logger.name), nrow(df.diff),
                                       min(df.diff$TIMESTAMP_UTC), max(df.diff$TIMESTAMP_UTC)),
                               x = 0.05, hjust = 0)

  p.empty <- ggplot2::ggplot() + ggplot2::theme_void()

  local({
    png(filename, width = 1280, height = 720)
    on.exit(dev.off())
    gridExtra::grid.arrange(p.comp, p.empty,
                            p.M, p.M.ARS,
                            p.M.ARD, p.M.ARDS,
                            layout_matrix = layout_matrix,
                            top = grob.title)
  })

  invisible(TRUE)

}

# report('BAOL009X_78680')
invisible(lapply(logger.names, report))
