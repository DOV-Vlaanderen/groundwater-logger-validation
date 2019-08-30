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
    'AO' = if (index == n) rep(0, n) # if last point, then all 0
           else rep(c(0, 1, -1, 0), c(index - 1L, 1L, 1L, n - index - 1L)),
    'LS' = rep(c(0, 1, 0), c(index - 1L, 1L, n - index)),
    'TC' = rep(c(0, 1), c(index - 1L, n - index + 1L))
  )
}

# indicator('AO', 1, 1)
# indicator('AO', 2, 5)
# indicator('LS', 1, 5)
# indicator('TC', 5, 5)
# indicator('AA', 5, 5)

decay <- function(index, decay, n) {
  c(rep(0, index - 1L), 1, -decay^(1:(n-index)))
}

# Dit is z_t - z_t-1 - ... = w_t ~ epsilon distributed
logL.base <- function(w, mu, sigma2) {
  -sum((w - mu)^2/(2*sigma2))
}

Optimizer <- function(z, types, indexes, mu, sigma2) {
  if (length(types) != length(indexes))
    stop('ERROR: types and indexes must have equal length.')

  n <- length(z)
  force(mu); force(sigma2)

  # M is the indicator matrix according to type
  # mapply costs 35 mus, while indicator 15 mus for n = 1000
  M <- if (length(types) > 0L)
    do.call(cbind, mapply(indicator, type = types, index = indexes, n = n, SIMPLIFY = FALSE))

  type.par.nr <- ifelse(types == 'TC', 2L, 1L)
  par.idx.delta <- cumsum(type.par.nr)[which(type.par.nr == 2L)]
  par.idx.alpha <- setdiff(1L:sum(type.par.nr), par.idx.delta)

  par.init <- rep(0, sum(type.par.nr))
  par.init[par.idx.delta] <- 0.7

  type.idx.tc <- which(types == 'TC')
  indexes.tc <- indexes[type.idx.tc]

  logL <- function(par) {

    if (length(types) == 0L) return(logL.base(w = z, mu = mu, sigma2 = sigma2))

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

    opt <- optim(par = par.init, fn = logL, method = 'L-BFGS-B',
                 lower = ll, upper = ul,
                 control = list('fnscale' = -1))

    structure(round(opt$value, digits = 6), 'par' = opt$par,
              'types' = types, 'indexes' = indexes)
  }

  list('optimize' = optimize)
}

#tmp <- Optimizer(z = rnorm(1), types = c('AO'), indexes = 1, mu = 0, sigma2 = 1)
#tmp <- Optimizer(z = rnorm(1000), types = NULL, indexes = NULL, mu = 0, sigma2 = 1)
#tmp <- Optimizer(z = rnorm(1000), types = c('AO'), indexes = 1, mu = 0, sigma2 = 1)
#tmp <- Optimizer(z = rnorm(1000), types = c('AO', 'LS'), indexes = c(1, 3), mu = 0, sigma2 = 1)
#tmp <- Optimizer(z = rnorm(1000), types = c('AO', 'TC', 'LS'), indexes = c(1, 2, 3), mu = 0, sigma2 = 100)
#tmp$optimize()

sweep <- function(x, last.result = NULL, mu, sigma2, outlier) {
  indexes <- which(outlier)
  last.types <- attr(last.result, 'types')
  last.indexes <- attr(last.result, 'indexes')

  fn <- function(type, index) {
    if (any(last.types == type & last.indexes == index)) return(last.result)
    O <- Optimizer(z = x, types = c(last.types, type), indexes = c(last.indexes, index),
                   mu = mu, sigma2 = sigma2)
    O$optimize()
  }

  res <- mapply(fn, index = indexes, type = 'AO', SIMPLIFY = FALSE)
  res <- c(res, mapply(fn, index = indexes, type = 'LS', SIMPLIFY = FALSE))

  res.idx.sorted <- order(sapply(res, function(x) x),
                          sapply(res, function(x) -sum(abs(attr(x, 'par')))),
                          decreasing = TRUE)

  res[[res.idx.sorted[1L]]]
}

lrtest <- function(logL0, logL, df.diff) {
  as.vector(1-pchisq(q = -2*(logL0 - logL), df = df.diff))
}

seeker <- function(x, mu, sigma2, outlier){
  logLres <- list()
  logLres[[1]] <- Optimizer(z = x, types = NULL, indexes = NULL, mu = mu, sigma2 = sigma2)$optimize()
  logLres[[2]] <- sweep(x, mu = mu, sigma2 = sigma2, outlier = outlier)

  repeat({
    last <- logLres[[length(logLres)]]
    # idealiter zouden alle permutaties moeten getest worden voor 2 parameters, en niet
    # met 1 fixed, want die 1 fixed is optimaal voor 1 parameter model, maar niet
    # noodzakelijk samengaand met een andere parameter.
    res <- sweep(x = x, last.result = last, mu = mu, sigma2 = sigma2, outlier = outlier)
    if (do.call(lrtest, list(last, res, 'df.diff' = 1L)) > 1E-8) break()
    logLres[[length(logLres) + 1L]] <- res
  })

  logLres
}

detect <- function(x, timestamps, nr.tail = 25) {
  idx <- order(timestamps)
  x <- x[idx]
  timestamps <- timestamps[idx]

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

  empty.df <- data.table::data.table('type' = character(), 'index' = integer())

  res <- mapply(start = drle[, start], end = drle[, end], FUN = function(start, end) {
    df <- dif[start:end, ]

    res <- seeker(x = df$vdiff, outlier = df$outlier, mu = df$mu, sigma2 = df$sigma2)
    res <- res[[length(res)]]

    data.frame('type' = attr(res, 'types'),
               'index' = attr(res, 'indexes') + start)
  }, SIMPLIFY = FALSE)

  res <- data.table::rbindlist(res)

  if (nrow(res) == 0L) empty.df else res[, index := idx[index]] # back to original idx
}

# set.seed(2019)
# detect(x = c(rnorm(20), 50, rnorm(19) + 50, -50, rnorm(50), 50, rnorm(5)),
#        timestamps = seq(as.POSIXct('2000-01-01'), as.POSIXct('2000-01-02'), by = '15 min'))

