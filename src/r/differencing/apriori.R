source('./differencing/functions.R')
source('./plot_zoomer.R')

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


logL <- function(alpha, ls_index_vec, ao_index_vec, x, mean = 0, sigma2) {
  if (length(alpha) != length(ls_index_vec) + length(ao_index_vec))
    stop('ERROR: alphas in logL do not match indices')

  alpha_ls <- if (length(ls_index_vec) > 0L) alpha[1:length(ls_index_vec)] else NULL
  alpha_ao <- if (length(ao_index_vec) > 0L) alpha[(1:length(ao_index_vec)) + length(ls_index_vec)] else NULL

  x[ls_index_vec] <- x[ls_index_vec] - alpha_ls
  x[ao_index_vec] <- x[ao_index_vec] - alpha_ao
  x[ao_index_vec+1] <- x[ao_index_vec+1] + alpha_ao

  #sum(log(kde.fit(x = x)))
  -sum((x - mean)^2/(2*sigma2))
}

logL(NULL, NULL, NULL, x = c(3, -8, 100, 12, 5), sigma2 = var(tmp))
optim(par = NULL, fn = logL, method = 'L-BFGS-B', ls_index_vec = NULL, ao_index_vec = NULL,
      x = c(3, -8, 100, 12, 5), control = list('fnscale' = -1), sigma2 = var(tmp))
optim(par = 0, fn = logL, method = 'L-BFGS-B', ls_index_vec = 3, ao_index_vec = NULL,
      x = c(3, -8, 100, 12, 5), control = list('fnscale' = -1), sigma2 = var(tmp))
optim(par = c(0, 0), fn = logL, method = 'L-BFGS-B', ls_index_vec = 3, ao_index_vec = 1,
      x = c(3, -8, 100, 12, 5), control = list('fnscale' = -1), sigma2 = var(tmp))

lrtest <- function(logL0, logL, df.diff) {
  as.vector(1-pchisq(q = -2*(logL0 - logL), df = df.diff))
}

lrtest(-26, -18, 1)

opt <- function(x, ls_index_vec = NULL, ao_index_vec = NULL, sigma2) {
  obj <- optim(par = rep(0, length(ls_index_vec) + length(ao_index_vec)),
               fn = logL, method = 'L-BFGS-B',
               ls_index_vec = ls_index_vec, ao_index_vec = ao_index_vec,
               x = x, control = list('fnscale' = -1), sigma2 = sigma2)

  structure(round(obj$value, digits = 6),
            'par' = obj$par, 'ls_index_vec' = ls_index_vec, 'ao_index_vec' = ao_index_vec)
}

seeker <- function(x){
  sigma2 <- var(tmp)
  logL0 <- opt(x, sigma2 = sigma2)
  print('logL0')
  print(logL0)


  logL1.vec <- c(lapply(1:length(x), function(i) opt(x = x, ls_index_vec = i, sigma2 = sigma2)),
                 lapply(1:(length(x)-1L), function(i) opt(x = x, ao_index_vec = i, sigma2 = sigma2)))

  logL1 <- logL1.vec[[which.max(logL1.vec)]]
  print('logL1')
  print(logL1)

  lrtest(logL0, logL1, 1L)
}

seeker(c(3, -8, 100, 12, 5))
seeker(c(3, -8, 100, -112, 5))

sweep <- function(x, given = NULL, sigma2) {
  ls_indexes <- setdiff(1:length(x), attr(given, 'ls_index_vec'))
  ao_indexes <- setdiff(1:(length(x)-1L), attr(given, 'ao_index_vec'))

  res <- c(lapply(ls_indexes, function(i) opt(x = x,
                                              ls_index_vec = c(i, attr(given, 'ls_index_vec')),
                                              ao_index_vec = attr(given, 'ao_index_vec'), sigma2 = sigma2)),
           lapply(ao_indexes, function(i) opt(x = x,
                                              ls_index_vec =  attr(given, 'ls_index_vec'),
                                              ao_index_vec = c(i, attr(given, 'ao_index_vec')), sigma2 = sigma2)))

  sorted.index <- order(sapply(res, function(x) x),
                        sapply(res, function(x) -sum(abs(attr(x, 'par')))),
                        decreasing = TRUE)

  res[[sorted.index[1L]]]
}


ctest <- function() {

}

seeker2 <- function(x, sigma2){
  logLres <- list()
  logLres[[1]] <- opt(x, sigma2 = sigma2)
  logLres[[2]] <- sweep(x, sigma2 = sigma2)
  repeat({
    last <- logLres[[length(logLres)]]
    # idealiter zouden alle permutaties moeten getest worden voor 2 parameters, en niet
    # met 1 fixed, want die 1 fixed is optimaal voor 1 parameter model, maar niet
    # noodzakelijk samengaand met een andere parameter.
    res <- sweep(x = x, given = last, sigma2 = sigma2)
    if (do.call(lrtest, list(last, res, 'df.diff' = 1L)) > 1E-8) break()
    logLres[[length(logLres) + 1L]] <- res
  })

  logLres
}

seeker2(c(3, -8, 100, 12, 5), sigma2 = var(tmp))
seeker2(c(3, -8, 100, -112, 5), sigma2 = var(tmp))
seeker2(c(3, -8, -100, -300, 5), sigma2 = var(tmp))

detect <- function(x, timestamps) {
  tsdiff <- as.integer(difftime(timestamps[-1L], timestamps[-length(timestamps)], units = 'secs'))
  vdiff <- diff(x)
  map <- lapply(unique(tsdiff), function(tsd) {
    dens <- apriori.diff.density(tsd)
    tr <- tresholds(dens)
    list('tsdiff' = tsd, 'lower.bound' = tr[1], 'upper.bound' = tr[2], 'sigma2' = var(dens))
  })
  map <- data.table::rbindlist(map)
  dif <- data.table::data.table('tsdiff' = tsdiff, 'vdiff' = vdiff)
  dif <- merge(dif, map, all.x = TRUE)
  dif[, 'outlier' := vdiff < lower.bound | vdiff > upper.bound]

  drle <- data.table::as.data.table(unclass(rle(dif$outlier)))
  drle[, 'cumsum' := cumsum(lengths)]
  drle[(values), 'start' := cumsum - lengths]
  drle[(values), 'end' := cumsum + 1L]
  drle[(values)]

  res <- mapply(start = drle[(values), start], end = drle[(values), end], FUN = function(start, end) {
    res <- seeker2(x = dif[start:end, vdiff], sigma2 = dif[start:end, sigma2])
    res <- res[[length(res)]]

    dLS <- if (is.null(attr(res, 'ls_index_vec'))) data.frame() else data.frame(type = 'LS', 'index' = attr(res, 'ls_index_vec') + start)
    dAO <- if (is.null(attr(res, 'ao_index_vec'))) data.frame() else data.frame(type = 'AO', 'index' = attr(res, 'ao_index_vec') + start)
    data.table::rbindlist(list(dLS, dAO))
  }, SIMPLIFY = FALSE)

  data.table::rbindlist(res)
}

df <- Logger('852_T4175')$df
det <- detect(x = df$PRESSURE_VALUE, timestamps = df$TIMESTAMP_UTC)
plot(x = 18000:19000, y = df[18000:19000, PRESSURE_VALUE])
points(x = 18671, y = df[18671, PRESSURE_VALUE], col = 'red')
points(x = 18731, y = df[18731, PRESSURE_VALUE], col = 'red')

df[, levelshifts := FALSE]
df[det[type == 'LS', index], levelshifts := TRUE]
plot_zoomer(gwloggeR:::scatterplot.levelshifts(x = df$PRESSURE_VALUE,
                                               levelshifts = df$levelshifts,
                                               timestamps = df$TIMESTAMP_UTC))
