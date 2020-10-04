# Likelihood and optimization of an AR(1) model with trend

set.seed(2020)

#' LogLikelihood of AR(1) model with drift and seasonality
#'
#' @param dsi Start index of the drift in range 1 to length(x).
#'
#' @keywords internal
logL <- function(mu, sigma, phi1, d, dsi, x) {
  dsi <- as.integer(dsi)
  stopifnot(dsi >= 1 && dsi <= length(x))
  n <- length(x)
  dt <- c(rep(0, dsi - 1), 1:(n - dsi + 1))
  stopifnot(length(dt) == n)
  M <- phi1*x[-n] + mu * (1 - phi1) + d*(dt[-1] - phi1*dt[-n])
  squared <- (x[-1] - M)^2
  sum(-squared/(2*sigma^2)) - (n - 1)*log(sigma * sqrt(2*pi))
}

logL(rnorm(100), mu = 0, sigma = 1, phi1 = 5, d = 2, dsi = 5)

logL.fn <- function(par, x, fixed) {
  do.call(logL, args = c(as.list(par), as.list(fixed), list(x = x)))
}

sim <- function(n, mu, sigma, phi1, d, dsi,
                init = if (dsi == 1) mu + d else mu,
                xt = NULL) {
  a <- rnorm(n, 0, sd = sigma)
  dt <- c(rep(0, dsi - 1), 1:(n - dsi + 1))
  if (is.null(xt)) {
    # xp is the previous x: x_{t-1}
    Reduce(f = function(xp, t) phi1*xp + mu*(1-phi1) + d*(dt[t] - phi1*dt[t-1]) + a[t], x = 2:n, init = init, accumulate = TRUE)
  } else {
    c(xt[1],
      phi1*xt[-n] + mu*(1-phi1) + d*(dt[-1] - phi1*dt[-n]) + a[-1])
  }
}

x.sim <- sim(10000, mu = 1032, sigma = sqrt(23), phi1 = 0.9, d = 0, dsi = 1)
plot(x.sim, type = 'l')

x.sim.trend <- sim(10000, mu = 1032, sigma = sqrt(23), phi1 = 0.9, d = 0.005, dsi = 1)
plot(x.sim.trend, type = 'l')


#' Fit the custom drift detection model
#'
#' If specific parameters are supplied, then they will be fixed.
#'
#' @keywords internal
fit <- function(x, mu = NULL, sigma = NULL, phi1 = NULL, d = NULL, dsi = NULL) {

  all.param.names <- c('mu', 'sigma', 'phi1', 'd', 'dsi')

  inits <- c(
    'mu'= median(x),
    'sigma' = var(x),
    'phi1' = 0,
    'd' = 0,
    'dsi' = 1
  )

  lower.bound <- c(
    'mu' = -Inf,
    'sigma' = 1e-7,
    'phi1' = -Inf,
    'd' = -Inf,
    'dsi' = 1
  )

  upper.bound <- c(
    'mu' = Inf,
    'sigma' = Inf,
    'phi1' = 1 - 1e-7,
    'd' = Inf,
    'dsi' = length(x)
  )

  fixed <- unlist(mget(all.param.names)[!sapply(mget(all.param.names), is.null)])
  param.names <- setdiff(all.param.names, names(fixed))

  opt <- optim(
    par = inits[param.names],
    fn = logL.fn,
    method = 'L-BFGS-B',
    x = x,
    lower = lower.bound[param.names],
    upper = upper.bound[param.names],
    fixed = fixed,
    control = list('fnscale' = -1, trace = 0)
  )

  # Add fixed parameters
  opt <- append(x = opt, values = list('par.fixed' = fixed), after = 1)

  opt
}

# Here specification of likelihood is tested with general arima model.
## x.sim
fit(x = x.sim) # -29826.68
fit(x = x.sim, dsi = 1)
fit(x = x.sim, d = 0, dsi = 1) # -29827.19

#forecast::auto.arima(y = x.sim, trace = TRUE)
fit.arima <- arima(x = x.sim, order = c(1, 0, 0))
fit.arima # -29830.47

## x.sim.trend
fit(x = x.sim.trend, dsi = 1) # -29876.3
fit.arima <- arima(x = x.sim.trend, order = c(1, 0, 0),
                   xreg = matrix(data = 1:length(x.sim.trend), dimnames = list(NULL, 'trend')))
fit.arima # -29879.61


