# Likelihood and optimization of an AR(1) model with trend

set.seed(2020)

logL <- function(mu, sigma, phi1, x) {
  n <- length(x)
  M <- phi1*x[-n] + mu * (1 - phi1)
  squared <- (x[-1] - M)^2
  sum(-squared/(2*sigma^2)) - (n - 1)*log(sigma * sqrt(2*pi))
}

logL.fn <- function(par, x, fixed) {
  do.call(logL, args = c(as.list(par), as.list(fixed), list(x = x)))
}

logL(rnorm(100), mu = 0, sigma = 1, phi1 = 5)

sim <- function(n, mu, sigma, phi1, init = mu) {
  a <- rnorm(n, 0, sd = sigma)
  Reduce(f = function(x1, a) phi1*x1 + mu*(1-phi1) + a, x = a, init = init, accumulate = TRUE)[-1]
}

x.sim <- sim(10000, mu = 1032, sigma = 23, phi1 = 0.9)
plot(x.sim, type = 'l')

#' Fit the custom drift detection model
#'
#' If specific parameters are supplied, then they will be fixed.
#'
#' @keywords internal
#'
fit <- function(x, mu = NULL, sigma = NULL, phi1 = NULL) {

  all.param.names <- c('mu', 'sigma', 'phi1')

  inits <- c(
    'mu'= 1013,
    'sigma' = 1,
    'phi1' = 0
  )

  lower.bound <- c(
    'mu' = -Inf,
    'sigma' = 1e-7,
    'phi1' = -Inf
  )

  upper.bound <- c(
    'mu' = Inf,
    'sigma' = Inf,
    'phi1' = Inf
  )

  fixed <- unlist(mget(all.param.names)[!sapply(mget(all.param.names), is.null)])
  param.names <- setdiff(all.param.names, names(fixed))

  optim(
    par = inits[param.names],
    fn = logL.fn,
    method = 'L-BFGS-B',
    x = x.sim,
    lower = lower.bound[param.names],
    upper = upper.bound[param.names],
    fixed = fixed,
    control = list('fnscale' = -1)
  )
}

fit(x = x.sim)
fit(x = x.sim, mu = 0)




#forecast::auto.arima(y = x.sim, trace = TRUE)
fit.arima <- arima(x = x.sim, order = c(1, 0, 0))
fit.arima
