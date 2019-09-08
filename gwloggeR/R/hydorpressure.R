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
# indicator('TC', 1, 1)
# indicator('AO', 2, 5)
# indicator('LS', 1, 5)
# indicator('TC', 5, 5)
# indicator('AA', 5, 5)

decay <- function(index, decay, n) {
  if (index > n) stop('ERROR: index larger than n.')
  exp.decay <- c(rep(0, index - 1L), decay^(0:(n-index)))
  exp.decay - c(0, exp.decay[-n])
}

decay(2, 0.7, 5)
decay(1, 0.7, 2)
decay(1, 0.7, 1)

# Dit is z_t - z_t-1 - ... = w_t ~ epsilon distributed
logL.base <- function(w, mu, sigma2) {
  -sum((w - mu)^2/(2*sigma2))
}

Optimizer.Result <- function(logLval, par, types, types.significant, indexes) {
  opt <- structure(round(logLval, digits = 4),
                   'par' = round(par, digits = 3),
                   'types' = types, 'types.significant' = types.significant,
                   'indexes' = indexes)
  class(opt) <- 'Optimizer.Result'
  opt
}

Optimizer <- function(z, types, indexes, mu, sigma2, par.init = NULL) {
  n <- length(z)

  if (length(types) != length(indexes))
    stop('ERROR: types and indexes must have equal length.')

  if (!(n == length(mu) || length(mu) == 1L))
    stop('ERROR: z and mu must have same length or length(mu) == 1.')

  if (!(n == length(sigma2) || length(sigma2) == 1L))
    stop('ERROR: z and sigma2 must have same length or length(sigma2) == 1.')

  # M is the indicator matrix according to type
  # mapply costs 35 mus, while indicator 15 mus for n = 1000
  M <- if (length(types) > 0L)
    do.call(cbind, mapply(indicator, type = types, index = indexes, n = n, SIMPLIFY = FALSE))

  type.par.nr <- ifelse(types == 'TC', 2L, 1L)
  par.idx.delta <- cumsum(type.par.nr)[which(type.par.nr == 2L)]
  par.idx.alpha <- setdiff(seq.int(1L, length.out = sum(type.par.nr)), par.idx.delta)

  if (is.null(par.init)) {
    par.init <- rep(0, sum(type.par.nr))
    par.init[par.idx.alpha] <- z[indexes]
    par.init[par.idx.delta] <- 0.7
  }

  type.idx.tc <- which(types == 'TC')
  indexes.tc <- indexes[type.idx.tc]

  set.par.init <- function(par.init) {
    assign(x = 'par.init', value = par.init,
           envir = parent.env(env = environment()))
  }

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

  types.significant <- function(par, p.val.trehold = 1e-5) {
    par.alpha <- par[par.idx.alpha]
    p.val <- pnorm(par.alpha, mean = mu[indexes], sd = sqrt(sigma2[indexes]))
    p.val < p.val.trehold | p.val > 1-p.val.trehold
  }

  optimize <- function() {
    ul <- rep(1e8, length(par.init))
    ll <- -ul

    ul[par.idx.delta] <- 0.90
    ll[par.idx.delta] <- 0.10

    parscale <- abs(par.init)
    parscale[par.idx.delta] <- 0.5 # doubles the range such that it is > 1
    parscale[parscale == 0] <- 10

    opt <- optim(par = par.init, fn = logL, method = 'L-BFGS-B',
                 lower = ll, upper = ul,
                 control = list('fnscale' = -1,
                                'parscale' = parscale))

    Optimizer.Result(logLval = round(opt$value, digits = 4),
                     par = round(opt$par, digits = 3),
                     types = types, types.significant = types.significant(opt$par),
                     indexes = indexes)
  }

  list('optimize' = optimize,
       'set.par.init' = set.par.init)
}

#tmp <- Optimizer(z = rnorm(1), types = c('AO'), indexes = 1, mu = 0, sigma2 = 1)
#tmp <- Optimizer(z = rnorm(1000), types = NULL, indexes = NULL, mu = 0, sigma2 = 1)
#tmp <- Optimizer(z = rnorm(1000), types = c('AO'), indexes = 1, mu = 0, sigma2 = 1)
#tmp <- Optimizer(z = rnorm(1000), types = c('AO', 'LS'), indexes = c(1, 3), mu = 0, sigma2 = 1)
#tmp <- Optimizer(z = rnorm(1000), types = c('AO', 'TC', 'LS'), indexes = c(1, 2, 3), mu = 0, sigma2 = 100)
# tmp <- Optimizer(z = X, types = 'TC', indexes = 26L, mu = MU, sigma2 = SIGMA2)
# tmp <- Optimizer(z = X, types = c('LS', 'TC'), indexes = c(26L, 26L), mu = MU, sigma2 = SIGMA2)
# tmp <- Optimizer(z = DF$vdiff, types = 'TC', indexes = 54L, mu = DF$mu, sigma2 = DF$sigma2)
# tmp$optimize()

ProgressTable <- function() {

  # index is df + 1L (since df can be 0)
  tbl <- list()
  df.base.swept <- 0L

  update <- function(result) {
    if (class(result) != 'Optimizer.Result') return()

    par <- attr(result, 'par')
    nr.par <- length(par)
    tbl.idx <- nr.par + 1L

    if (tbl.idx > length(tbl)
        || result > tbl[[tbl.idx]]
        || (result == tbl[[tbl.idx]]
            && sum(abs(par)) < sum(abs(attr(tbl[[tbl.idx]], 'par'))))) {
      tbl[[tbl.idx]] <<- result
      # reduce current df.base.swept if parameters at the same level or below change
      if (df.base.swept >= nr.par)
        df.base.swept <<- max(0L, nr.par - 1L)
    }
  }

  get <- function(df = NULL) { # df = nr.parameters
    if (is.null(df)) return(tbl)
    if (df + 1L > length(tbl)) return(NULL)
    tbl[[df + 1L]]
  }

  get.df.base.swept <- function() df.base.swept

  set.df.base.swept <- function(df) df.base.swept <<- df

  list('update' = update,
       'get' = get,
       'get.df.base.swept' = get.df.base.swept,
       'set.df.base.swept' = set.df.base.swept)
}

sweep <- function(x, base.types = NULL, base.indexes = NULL, base.par = NULL,
                  sweep.indexes, types, mu, sigma2) {

  sweeper <- function(type, index, base.types, base.indexes, base.par) {

    # Don't check if new type/index is allready in base
    if (any(type == base.types & index == base.indexes)) return(NULL)

    O <- Optimizer(z = x, mu = mu, sigma2 = sigma2,
                   types = c(base.types, type),
                   indexes = c(base.indexes, index))

    O$set.par.init(c(base.par, if (type == 'TC') c(0, 0.7) else 0))

    opt <- O$optimize()

    # if last insignificant: OK, else remove and recurse
    par <- attr(opt, 'par')
    types <- attr(opt, 'types')
    types.sig <- attr(opt, 'types.significant')
    if (length(base.types) > 1L && any(!types.sig[-length(types.sig)])) {
      base.types.sig <- types.sig[-length(types.sig)]
      sweeper(type, index,
              base.types = base.types[base.types.sig],
              base.indexes = base.indexes[base.types.sig],
              base.par = par[1:length(base.par)][rep(base.types.sig, ifelse(base.types == 'TC', 2L, 1L))])
    } else {
      opt
    }
  }

  # zou moeten nagegeken worden of index/type combo niet reeds is gebruikt.
  mapply(sweeper, index = rep(sweep.indexes, times = length(types)),
         type = rep(types, each = length(sweep.indexes)),
         base.types = list(base.types), base.indexes = list(base.indexes),
         base.par = list(base.par),
         SIMPLIFY = FALSE)
}

lrtest <- function(logL0, logL, df.diff) {
  as.vector(1-pchisq(q = -2*(logL0 - logL), df = df.diff))
}


seeker <- function(x, mu, sigma2, outlier, types){
  sweep.indexes <- which(outlier)
  pt <- ProgressTable()
  pt$update(Optimizer(z = x, types = NULL, indexes = NULL, mu = mu, sigma2 = sigma2)$optimize())
  lapply(sweep(x, mu = mu, sigma2 = sigma2, types = types, sweep.indexes = sweep.indexes), pt$update)

  repeat({
    # idealiter zouden alle permutaties moeten getest worden voor 2 parameters, en niet
    # met 1 fixed, want die 1 fixed is optimaal voor 1 parameter model, maar niet
    # noodzakelijk samengaand met een andere parameter.
    base.df <- pt$get.df.base.swept() + 1L
    base <- pt$get(base.df)
    pt$set.df.base.swept(base.df)

    lapply(sweep(x = x,
                 base.types = attr(base, 'types'),
                 base.indexes = attr(base, 'indexes'),
                 base.par = attr(base, 'par'),
                 mu = mu, sigma2 = sigma2,
                 types = types,
                 sweep.indexes = sweep.indexes), pt$update)

    if (base.df != pt$get.df.base.swept()) next() # if df.base changed: retry
    if (is.null(pt$get(base.df + 1L))) break() # no more new options
    if (lrtest(logL0 = base, logL = pt$get(base.df + 1L), df.diff = 1L) > 1E-4) break()
  })

  pt$get(pt$get.df.base.swept())
}

detect <- function(x, timestamps, types = c('AO', 'LS', 'TC'), nr.tail = 25) {
  stopifnot(length(x) == length(timestamps))
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

  empty.df <- data.table::data.table('type' = character(), 'index' = integer(),
                                     'alpha' = numeric(), 'delta' = numeric())

  res <- mapply(start = drle[, start], end = drle[, end], FUN = function(start, end) {
    df <- dif[start:end, ]

    res <- seeker(x = df$vdiff, outlier = df$outlier, types = types, mu = df$mu, sigma2 = df$sigma2)

    type.par.nr <- ifelse(attr(res, 'types') == 'TC', 2L, 1L)
    par.idx.delta <- cumsum(type.par.nr)[which(type.par.nr == 2L)]
    par.idx.alpha <- setdiff(seq.int(1L, length.out = sum(type.par.nr)), par.idx.delta)
    par.delta <- rep(NA, length(type.par.nr))
    par.delta[attr(res, 'types') == 'TC'] <- attr(res, 'par')[par.idx.delta]

    data.frame('type' = attr(res, 'types'),
               'index' = attr(res, 'indexes') + start,
               'alpha' = attr(res, 'par')[par.idx.alpha],
               'delta' = par.delta)
  }, SIMPLIFY = FALSE)

  res <- data.table::rbindlist(res)

  if (nrow(res) == 0L) empty.df else res[, index := idx[index]] # back to original idx
}

# set.seed(2019)
# print(detect(x = c(-50, rnorm(18), 50, rnorm(19) + 50, -50, rnorm(4), rnorm(45) + 100*0.8^(0:44), 50, rnorm(7)),
#              timestamps = seq(as.POSIXct('2000-01-01'), as.POSIXct('2000-01-02'), by = '15 min'))[])

