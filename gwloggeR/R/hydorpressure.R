#' @importFrom data.table :=
#' @keywords internal
apriori.hydropressure.difference.samples <- function(interval.sec) {
  # interval.sec adjustment: if > 24h, then adjust so it is divisible by 15min
  if (interval.sec > 60*60*24)
    interval.sec <- round(interval.sec/(60*15))*60*15
  else if (interval.sec > 60*60*1) # if > 1h, then divisible by 5 min
    interval.sec <- round(interval.sec/(60*5))*60*5
  else # else divisible by 1min
    interval.sec <- round(interval.sec/(60))*60

  if (interval.sec > 60*60*24*30) # if larger than a month, then reduce to month
    interval.sec <- 60*60*24*30

  # Unique time differences (DIFF.SEC) per FILE.
  df.map <- hydropressure[, .(DIFF.SEC = unique(as.numeric(diff(TIMESTAMP_UTC), units = 'secs'))),
                          by = FILE]

  # Make sure that DIFF.SEC is unique per FILE.
  if (length(unique(df.map[, FILE])) != nrow(df.map))
    stop('ERROR: hydropressure time differences not unique per file.')

  # You can not use e.g. 15 min frequency data to simulate 20 min data.
  # You can not use e.g. 15 min frequency data to simulate 5 min data (i.e. higher freq).
  df.map <- df.map[interval.sec %% DIFF.SEC == 0,]
  if (nrow(df.map) == 0L)
    stop(sprintf('ERROR: chosen interval (%s) not a multiple of any DIFF.SEC.', interval.sec))
  df.hp <- hydropressure[J(df.map[, .(FILE)]),]

  # Differences generation
  df.hp[, CUM.DIFF.SEC := as.numeric(difftime(TIMESTAMP_UTC, min(TIMESTAMP_UTC)), units = 'secs'),
        by = FILE]

  # Point selection
  df.hp <- df.hp[CUM.DIFF.SEC %% interval.sec == 0, ]

  df.hp[, .('DIFF.VALUE' = diff(PRESSURE_VALUE)), by = FILE][, DIFF.VALUE]
}

indicator <- function(type = c('AO', 'LS', 'TC'), index, n) {
  if (index > n) stop('ERROR: index larger than n.')
  type <- match.arg(type)

  switch (type,
    'AO' = rep(c(0, 1, -1, 0), c(index - 1L, 1L, 1L, n - index - 1L)),
    'LS' = rep(c(0, 1, 0), c(index - 1L, 1L, n - index)),
    'TC' = rep(c(0, 1), c(index - 1L, n - index + 1L))
  )
}

# indicator('AO', 2, 5)
# indicator('LS', 1, 5)
# indicator('TC', 5, 5)
# indicator('AA', 5, 5)

decay <- function(index, decay, n) {
  c(rep(0, index - 1L), 1, -decay^(1:(n-index)))
}

multiplier <- function(type = c('AO', 'LS', 'TC'), index, n, alpha, decay = NA) {
  if (type == 'TC' && is.na(decay)) stop('ERROR: decay must be specified in case of TC.')
  type <- match.arg(type)

  switch (type,
          'AO' = alpha,
          'LS' = alpha,
          'TC' = alpha * c(rep(0, index - 1L), 1, -decay^(1:(n-index)))
  )
}

# multiplier('TC', 3, 5, 1, 0.7)

# Dit is z_t - z_t-1 - ... = w_t ~ epsilon distributed
logL.base <- function(w, mu, sigma2) {
  -sum((w - mu)^2/(2*sigma2))
}

w <- function(z, types, indexes, alphas, decays) {
  Reduce(x = 1:length(types), f = function(x, i){
    x + indicator(type = types[i], index = indexes[i], n = length(z)) *
      multiplier(type = types[i], index = indexes[i], n = length(z), alpha = alphas[i], decay = decays[i])
  }, init = z)
}

# w(rep(0, 10), types = 'AO', indexes = 2, alphas = 1, decays = NA)
# w(rep(0, 10), types = c('AO', 'TC'), indexes = c(2, 5), alphas = c(1, 1), decays = c(NA, 0.7))
# logL.base(w(rep(0, 10), types = c('AO', 'TC'), indexes = c(2, 5), alphas = c(1, 1), decays = c(NA, 0.7)), 0, 1)

logL <- function(alpha, ls_index_vec, ao_index_vec, x, mu, sigma2) {
  if (length(alpha) != length(ls_index_vec) + length(ao_index_vec))
    stop('ERROR: alphas in logL do not match indices')

  alpha_ls <- if (length(ls_index_vec) > 0L) alpha[1:length(ls_index_vec)] else NULL
  alpha_ao <- if (length(ao_index_vec) > 0L) alpha[(1:length(ao_index_vec)) + length(ls_index_vec)] else NULL

  x[ls_index_vec] <- x[ls_index_vec] - alpha_ls
  x[ao_index_vec] <- x[ao_index_vec] - alpha_ao
  x[ao_index_vec+1] <- x[ao_index_vec+1] + alpha_ao

  -sum((x - mu)^2/(2*sigma2))
}

fn <- function(){
  1+1
}

fn2 <- function() {
  fn()
}

Optimizer <- function(z, types, indexes, mu, sigma2) {
  n <- length(z)

  # M is the indicator matrix according to type
  # mapply costs 35 mus, while indicator 15 mus for n = 1000
  M <- mapply(indicator, type = types, index = indexes, n = n)

  type.par.nr <- ifelse(types == 'TC', 2L, 1L)
  par.idx.delta <- cumsum(type.par.nr)[which(type.par.nr == 2L)]
  par.idx.alpha <- (1L:sum(type.par.nr))[-par.idx.delta]

  par.init <- rep(0, sum(type.par.nr))
  par.init[par.idx.delta] <- 0.7

  type.idx.tc <- which(types == 'TC')
  indexes.tc <- indexes[which(types == 'TC')]

  logL <- function(par) {

    # matrix multiplication columnwise with a vector
    # https://stackoverflow.com/a/32364355/1548942
    alpha <- par[par.idx.alpha]
    M <- M * rep(alpha, rep(nrow(M), length(alpha)))

    if (length(par.idx.delta) > 0L) {
      decays <- mapply(decay, index = indexes.tc, decay = par[par.idx.delta], n = n)
      M[, type.idx.tc] <- M[, type.idx.tc] * decays
    }

    logL.base(w = z - rowSums(M), mu = mu, sigma2 = sigma2)
  }

  optimize <- function() {
    ul <- rep(Inf, length(par.init))
    ll <- -ul

    ul[par.idx.delta] <- 1
    ll[par.idx.delta] <- 0

    opt <- optim(par = par.init, fn = logL, method = 'L-BFGS-B', lower = ll, upper = ul)

    structure(round(opt$value, digits = 6), 'par' = opt$par)
  }

  list('optimize' = optimize)
}

#Optimizer(z = rnorm(1000), types = c('AO', 'LS'), indexes = c(1, 3), mu = 0, sigma2 = 1)
tmp <- Optimizer(z = rnorm(1000), types = c('AO', 'TC', 'LS'), indexes = c(1, 2, 3), mu = 0, sigma2 = 100)
#tmp$optimize()

opt <- function(x, ls_index_vec = NULL, ao_index_vec = NULL, mu, sigma2) {
  obj <- optim(par = rep(0, length(ls_index_vec) + length(ao_index_vec)),
               fn = logL, method = 'L-BFGS-B',
               ls_index_vec = ls_index_vec, ao_index_vec = ao_index_vec,
               x = x, control = list('fnscale' = -1),
               mu = mu, sigma2 = sigma2)

  structure(round(obj$value, digits = 6), 'par' = obj$par,
            'ls_index_vec' = ls_index_vec, 'ao_index_vec' = ao_index_vec)
}

sweep <- function(x, given = NULL, mu, sigma2) {
  ls_indexes <- setdiff(1:length(x), attr(given, 'ls_index_vec'))
  ao_indexes <- setdiff(1:(length(x)-1L), attr(given, 'ao_index_vec'))

  res <- c(lapply(ls_indexes, function(i) opt(x = x,
                                              ls_index_vec = c(i, attr(given, 'ls_index_vec')),
                                              ao_index_vec = attr(given, 'ao_index_vec'),
                                              mu = mu, sigma2 = sigma2)),
           lapply(ao_indexes, function(i) opt(x = x,
                                              ls_index_vec =  attr(given, 'ls_index_vec'),
                                              ao_index_vec = c(i, attr(given, 'ao_index_vec')),
                                              mu = mu, sigma2 = sigma2)))

  sorted.index <- order(sapply(res, function(x) x),
                        sapply(res, function(x) -sum(abs(attr(x, 'par')))),
                        decreasing = TRUE)

  res[[sorted.index[1L]]]
}

lrtest <- function(logL0, logL, df.diff) {
  as.vector(1-pchisq(q = -2*(logL0 - logL), df = df.diff))
}

seeker2 <- function(x, mu, sigma2){
  logLres <- list()
  logLres[[1]] <- opt(x, mu = mu, sigma2 = sigma2)
  logLres[[2]] <- sweep(x, mu = mu, sigma2 = sigma2)
  repeat({
    last <- logLres[[length(logLres)]]
    # idealiter zouden alle permutaties moeten getest worden voor 2 parameters, en niet
    # met 1 fixed, want die 1 fixed is optimaal voor 1 parameter model, maar niet
    # noodzakelijk samengaand met een andere parameter.
    res <- sweep(x = x, given = last, mu = mu, sigma2 = sigma2)
    if (do.call(lrtest, list(last, res, 'df.diff' = 1L)) > 1E-8) break()
    logLres[[length(logLres) + 1L]] <- res
  })

  logLres
}

detect <- function(x, timestamps, nr.tail = 25) {
  idx <- order(timestamps)
  x <- x[idx]
  timestamps <- timestamps[idx]
  browser()

  tsdiff <- as.numeric(diff(timestamps), units = 'secs')
  vdiff <- diff(x)
  ftab <- table(tsdiff)
  map <- mapply(interval.sec = as.numeric(names(ftab)), n = ftab,
                FUN = function(interval.sec, n) {
                  dens <- apriori.hydropressure.difference.samples(interval.sec)
                  tr <- range(dens)
                  list('tsdiff' = interval.sec,
                       'lower.bound' = tr[1], 'upper.bound' = tr[2],
                       'mu' = mean(dens), 'sigma2' = var(dens),
                       'n.dens' = length(dens),
                       'c' = c.norm.optimal(alpha = 1/2000, n = n, type = 'two.sided'))
  }, SIMPLIFY = FALSE)
  map <- data.table::rbindlist(map)
  dif <- data.table::data.table('tsdiff' = tsdiff, 'vdiff' = vdiff)
  dif <- merge(dif, map, all.x = TRUE, sort = FALSE)
  dif[, 'outlier' := abs(vdiff) > pmax(abs(lower.bound), abs(upper.bound))*1.2]

  drle <- data.table::as.data.table(unclass(rle(dif$outlier)))
  drle[, 'ends' := cumsum(lengths)]
  drle[-1L, 'traverse' := lengths < nr.tail | values]
  drle[c(1L, .N), 'traverse' := values]
  drle[, 'traverse.id' := data.table::rleid(traverse)]
  drle <- drle[(traverse),
               .('start' = ends[1L] - lengths[1L] + 1L,
                 'end' = pmin(ends[length(ends)] + nr.tail, nrow(dif))),
               by = traverse.id]
  drle

  empty.df <- data.table::data.table('type' = character(), 'index' = integer())

  res <- mapply(start = drle[, start], end = drle[, end], FUN = function(start, end) {
    res <- seeker2(x = dif[start:end, vdiff], mu = dif[start:end, mu], sigma2 = dif[start:end, sigma2])
    res <- res[[length(res)]]

    dLS <- if (is.null(attr(res, 'ls_index_vec'))) empty.df else data.frame(type = 'LS', 'index' = attr(res, 'ls_index_vec') + start)
    dAO <- if (is.null(attr(res, 'ao_index_vec'))) empty.df else data.frame(type = 'AO', 'index' = attr(res, 'ao_index_vec') + start)
    data.table::rbindlist(list(dLS, dAO))
  }, SIMPLIFY = FALSE)

  res <- data.table::rbindlist(res)

  if (nrow(res) == 0L) empty.df else res[, index := idx[index]] # back to original idx
}

# detect(x = c(rnorm(20), 50, rnorm(19) + 50, -50, rnorm(50), 50, rnorm(5)),
#        timestamps = seq(as.POSIXct('2000-01-01'), as.POSIXct('2000-01-02'), by = '15 min'))

