source('./differencing/functions.R')

apriori.diff.density <- function(interval.sec, start.time = NULL) {
  apriori.data <- readRDS('./differencing/apriori_hydrostatic_pressure_data_selection.rds')

  unlist(unname(lapply(apriori.data, FUN = differentiate2, interval.sec = interval.sec)))
}

tmp <- apriori.diff.density(interval.sec = 15*60)
hist(tmp, breaks = 100)

qqnorm(tmp)
qqline(tmp)

# tresholds
min(tmp)
max(tmp)

tresholds <- function(samples) {
  range(samples)
}

tresholds(tmp)
hist(tmp, breaks = 1000)
abline(v = tresholds(tmp))

# AO outlier: 2 consequtive points are outlier on left and right side of the distribution

detect_ao <- function(x, y, tresholds) {


}


kde.dnorm <- function(x, h, data) mean(dnorm(x - data, 0, h))
#h <- bw.nrd(x = tmp)
h <- 2
kde.fit <- Vectorize(function(x) kde.dnorm(x = x, h = h, data = tmp))
hist(tmp, breaks = 100, probability = TRUE)
curve(kde.fit, from = -10, to = 25, add = TRUE, n = 1000, col = 'red')
curve({dnorm(x, mean = mean(tmp), sd = sd(tmp))}, from = -10, to = 25, add = TRUE, n = 1000, col = 'blue')


logL <- function(alpha, ls_index_vec, ao_index_vec, x) {
  if (length(alpha) != length(ls_index_vec) + length(ao_index_vec))
    stop('ERROR: alphas in logL do not match indices')

  alpha_ls <- if (length(ls_index_vec) > 0L) alpha[1:length(ls_index_vec)] else NULL
  alpha_ao <- if (length(ao_index_vec) > 0L) alpha[(1:length(ls_index_vec)) + length(ls_index_vec)] else NULL

  x[ls_index_vec] <- x[ls_index_vec] - alpha_ls
  x[ao_index_vec] <- x[ao_index_vec] - alpha_ao
  x[ao_index_vec+1] <- x[ao_index_vec+1] + alpha_ao

  #sum(log(kde.fit(x = x)))
  -sum((x - mean(tmp))^2)/(2*var(tmp))
}

logL(NULL, NULL, NULL, x = c(3, -8, 100, 12, 5))
optim(par = NULL, fn = logL, method = 'L-BFGS-B', ls_index_vec = NULL, ao_index_vec = NULL, x = c(3, -8, 100, 12, 5), control = list('fnscale' = -1))
optim(par = 0, fn = logL, method = 'L-BFGS-B', ls_index_vec = 3, ao_index_vec = NULL, x = c(3, -8, 100, 12, 5), control = list('fnscale' = -1))
optim(par = c(0, 0), fn = logL, method = 'L-BFGS-B', ls_index_vec = 3, ao_index_vec = 1, x = c(3, -8, 100, 12, 5), control = list('fnscale' = -1))

lrtest <- function(logL0, logL, df.diff) {
  as.vector(1-pchisq(q = -2*(logL0 - logL), df = df.diff))
}

lrtest(-26, -18, 1)

opt <- function(x, ls_index_vec = NULL, ao_index_vec = NULL) {
  obj <- optim(par = rep(0, length(ls_index_vec) + length(ao_index_vec)),
               fn = logL, method = 'L-BFGS-B',
               ls_index_vec = ls_index_vec, ao_index_vec = ao_index_vec,
               x = x, control = list('fnscale' = -1))

  structure(obj$value, 'par' = obj$par, 'ls_index_vec' = ls_index_vec, 'ao_index_vec' = ao_index_vec)
}

seeker <- function(x){
  logL0 <- opt(x)
  print('logL0')
  print(logL0)

  logL1.vec <- c(lapply(1:length(x), function(i) opt(x = x, ls_index_vec = i)),
                 lapply(1:(length(x)-1L), function(i) opt(x = x, ao_index_vec = i)))

  logL1 <- logL1.vec[[which.max(logL1.vec)]]
  print('logL1')
  print(logL1)

  lrtest(logL0, logL1, 1L)
}

seeker(c(3, -8, 100, 12, 5))
seeker(c(3, -8, 100, -112, 5))
