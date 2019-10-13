#' @keywords internal
#'
hydropressure.timestamp.validation <- function(timestamps, x) {
  if (is.null(timestamps)) stop('ERROR: for hydrostatic pressure one needs to supply timestamps.')
  if (length(timestamps) != length(x)) stop('ERROR: x and timestamps must have same length.')
  assert.timestamp(timestamps)
  assert.notna.timestamp(timestamps)
}


#' @importFrom data.table :=
#' @keywords internal
#'
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
  df.map <- hydropressure[, list(DIFF.SEC = unique(as.numeric(diff(TIMESTAMP_UTC), units = 'secs'))),
                          by = FILE]

  # Make sure that DIFF.SEC is unique per FILE.
  if (length(unique(df.map[, FILE])) != nrow(df.map))
    stop('ERROR: hydropressure time differences not unique per file.')

  # You can not use e.g. 15 min frequency data to simulate 20 min data.
  # You can not use e.g. 15 min frequency data to simulate 5 min data (i.e. higher freq).
  df.map <- df.map[interval.sec %% DIFF.SEC == 0,]
  if (nrow(df.map) == 0L)
    stop(sprintf('ERROR: chosen interval (%s) not a multiple of any DIFF.SEC.', interval.sec))
  df.hp <- hydropressure[J(df.map[, list(FILE)]),]

  # Differences generation
  df.hp[, CUM.DIFF.SEC := as.numeric(difftime(TIMESTAMP_UTC, min(TIMESTAMP_UTC)), units = 'secs'),
        by = FILE]

  # Point selection
  df.hp <- df.hp[CUM.DIFF.SEC %% interval.sec == 0, ]

  df.hp[, list('DIFF.VALUE' = diff(PRESSURE_VALUE)), by = FILE][, DIFF.VALUE]
}


#' @title Indicator function
#' @description Indicator function for an event (AO, LS, etc.).
#' @keywords internal
#'
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


#' @title Decay function for x
#' @description Decay function for TC exponential decay when the model is written
#' in function of x only.
#' @keywords internal
#'
decay <- function(index, decay, n) {
  if (index > n) stop('ERROR: index larger than n.')
  exp.decay <- c(rep(0, index - 1L), decay^(0:(n-index)))
  exp.decay - c(0, exp.decay[-n])
}


logL.base <- function(dz, mu, sigma2) {
  -sum((dz - mu)^2/(2*sigma2))
}


Optimizer.Result <- function(logLval, par, types, types.significant, indexes, optim) {
  opt <- structure(round(logLval, digits = 4),
                   'par' = round(par, digits = 3),
                   'types' = types, 'types.significant' = types.significant,
                   'indexes' = indexes, 'optim' = optim)
  class(opt) <- 'Optimizer.Result'
  opt
}


#' @keywords internal
#'
Optimizer <- function(dx, types, indexes, mu, sigma2, par.init = NULL) {
  n <- length(dx)

  if (length(types) != length(indexes))
    stop('ERROR: types and indexes must have equal length.')

  if (!(n == length(mu)))
    stop('ERROR: dx and mu must have same length.')

  if (!(n == length(sigma2)))
    stop('ERROR: dx and sigma2 must have same length.')

  # M is the indicator matrix according to type
  # mapply costs 35 mus, while indicator 15 mus for n = 1000
  M <- if (length(types) > 0L)
    do.call(cbind, mapply(indicator, type = types, index = indexes, n = n, SIMPLIFY = FALSE))

  type.par.nr <- ifelse(types == 'TC', 2L, 1L)
  par.idx.delta <- cumsum(type.par.nr)[which(type.par.nr == 2L)]
  par.idx.alpha <- setdiff(seq.int(1L, length.out = sum(type.par.nr)), par.idx.delta)

  if (is.null(par.init)) {
    par.init <- rep(0, sum(type.par.nr))
    par.init[par.idx.alpha] <- dx[indexes]
    par.init[par.idx.delta] <- 0.7
  }

  type.idx.tc <- which(types == 'TC')
  indexes.tc <- indexes[type.idx.tc]

  set.par.init <- function(par.init) {
    if (length(par.init) != sum(type.par.nr))
      stop(sprintf('ERROR: %s init parameters suplied, but one needs %s',
                   length(par.init), sum(type.par.nr)))
    assign(x = 'par.init', value = par.init,
           envir = parent.env(env = environment()))
  }

  logL <- function(par) {

    if (length(types) == 0L) return(logL.base(dz = dx, mu = mu, sigma2 = sigma2))

    # matrix multiplication columnwise with a vector
    # https://stackoverflow.com/a/32364355/1548942
    alpha <- par[par.idx.alpha]
    M <- M * rep(alpha, rep(nrow(M), length(alpha)))

    if (length(par.idx.delta) > 0L) {
      decays <- mapply(decay, index = indexes.tc, decay = par[par.idx.delta], n = n)
      M[, type.idx.tc] <- M[, type.idx.tc] * decays
    }

    logL.base(dz = dx - rowSums(M), mu = mu, sigma2 = sigma2)
  }

  types.significant <- function(par, p.val.trehold = 1e-5) {
    if (length(par) == 0L) return(NULL)
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
                     indexes = indexes,
                     optim = opt)
  }

  list('optimize' = optimize,
       'set.par.init' = set.par.init)
}


#' @keywords internal
#'
ProgressTable <- function() {

  # index is df + 1L (since df can be 0)
  resultmap <- new.env(parent = environment())
  df.opt <- list()
  df.base.swept <- 0L

  interface <- new.env(parent = environment())

  key <- function(types, indexes, raw = FALSE) {
    if (length(types) == 0L) idx <- NULL
    else idx <- order(types, indexes)
    key <- list('types' = types[idx], 'indexes' = indexes[idx])
    if (raw) serialize(key, connection = NULL, ascii = FALSE)
    else key
  }

  hashkey <- function(types, indexes) {
    hashkey <- digest::digest(key(types, indexes, raw = TRUE),
                              serialize = FALSE, algo = 'md5')
  }

  interface$update <- function(result) {
    if (class(result) != 'Optimizer.Result') return()

    key <- hashkey(types = attr(result, 'types'), indexes = attr(result, 'indexes'))
    resultmap[[key]] <- result

    par <- attr(result, 'par')
    nr.par <- length(par)
    df.opt.idx <- nr.par + 1L

    if (df.opt.idx > length(df.opt)
        || result > df.opt[[df.opt.idx]]
        || (result == df.opt[[df.opt.idx]]
            && sum(abs(par)) < sum(abs(attr(df.opt[[df.opt.idx]], 'par'))))) {
      df.opt[[df.opt.idx]] <<- result
      # reduce current df.base.swept if parameters at the same level or below change
      if (df.base.swept >= nr.par)
        df.base.swept <<- max(0L, nr.par - 1L)
    }
  }

  interface$exists <- function(types, indexes) {
    exists(hashkey(types = types, indexes = indexes), resultmap)
  }

  interface$get.best.result <- function(df = NULL) { # df = nr.parameters
    if (is.null(df)) return(df.opt)
    if (df + 1L > length(df.opt)) return(NULL)
    df.opt[[df + 1L]]
  }

  interface$get.result <- function(types, indexes) {
    resultmap[[hashkey(types = types, indexes = indexes)]]
  }

  interface$get.df.base.swept <- function() {df.base.swept}

  interface$set.df.base.swept <- function(df) {df.base.swept <<- df}

  interface
}


#' @keywords internal
#'
sweep <- function(x, base.types = NULL, base.indexes = NULL, base.par = NULL,
                  sweep.indexes, types, mu, sigma2, pt) {

  sweeper <- function(type, index, base.types, base.indexes, base.par) {

    types <- c(base.types, type)
    indexes <- c(base.indexes, index)

    # Don't check if new type/index is allready in base
    if (any(type == base.types & index == base.indexes)) return(NULL)

    # Don't check if allready exists in progress table
    if (pt$exists(types = types, indexes = indexes)) return(NULL)

    # Optimize
    O <- Optimizer(dx = x, mu = mu, sigma2 = sigma2,
                   types = types,
                   indexes = indexes)
    O$set.par.init(c(base.par, if (type == 'TC') c(0, 0.7) else 0))
    opt <- O$optimize()

    # if last insignificant: OK, else remove and recurse
    types.sig <- attr(opt, 'types.significant')
    opt.rec <- if (length(base.types) > 1L && any(!types.sig[-length(types.sig)])) {
      par <- attr(opt, 'par')
      base.types.sig <- types.sig[-length(types.sig)]
      sweeper(type, index,
              base.types = base.types[base.types.sig],
              base.indexes = base.indexes[base.types.sig],
              base.par = par[1:length(base.par)][rep(base.types.sig, ifelse(base.types == 'TC', 2L, 1L))])
    }

    # if there is allready a type associated with the index, remove and retry
    opt.short <- if (index %in% base.indexes) {
      base.types.rm <- index == base.indexes
      base.par.rm <- rep(base.types.rm, ifelse(base.types == 'TC', 2L, 1L))
      sweeper(type = type, index = index,
              base.types = base.types[!base.types.rm],
              base.indexes = base.indexes[!base.types.rm],
              base.par = base.par[!base.par.rm])
    }

    list(opt, opt.rec, opt.short)
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


#' @keywords internal
#'
seeker <- function(x, mu, sigma2, outlier, types){
  sweep.indexes <- which(outlier)
  pt <- ProgressTable()
  pt$update(Optimizer(dx = x, types = NULL, indexes = NULL, mu = mu, sigma2 = sigma2)$optimize())
  invisible(rapply(object = sweep(x, mu = mu, sigma2 = sigma2,
                                  types = types, sweep.indexes = sweep.indexes,
                                  pt = pt),
                   f = pt$update, classes = 'Optimizer.Result'))

  repeat({
    # idealiter zouden alle permutaties moeten getest worden voor 2 parameters, en niet
    # met 1 fixed, want die 1 fixed is optimaal voor 1 parameter model, maar niet
    # noodzakelijk samengaand met een andere parameter.
    base.df <- pt$get.df.base.swept() + 1L
    base <- pt$get.best.result(base.df)
    pt$set.df.base.swept(base.df)

    invisible(rapply(object = sweep(x = x,
                                    base.types = attr(base, 'types'),
                                    base.indexes = attr(base, 'indexes'),
                                    base.par = attr(base, 'par'),
                                    mu = mu, sigma2 = sigma2,
                                    types = types,
                                    sweep.indexes = sweep.indexes,
                                    pt = pt),
                     f = pt$update, classes = 'Optimizer.Result'))

    if (base.df != pt$get.df.base.swept()) next() # if df.base changed: retry
    if (is.null(pt$get.best.result(base.df + 1L))) break() # no more new options
    if (lrtest(logL0 = base, logL = pt$get.best.result(base.df + 1L), df.diff = 1L) > 1E-4) break()
  })

  pt$get.best.result(pt$get.df.base.swept())
}


#' @title Events object
#' @description Events is a dataframe object that holds the indexes,
#' corresponding types and parameters. The function is vectorized.
#' @param results a list of one or multiple Optimizer.Result objects.
#' @param index.offsets starting index of window based on original data.
#' @param n length of the original x vector
#' @keywords internal
#'
Events <- function(results, index.offsets, n) {

  # Results are local windows, so index.offsets of windows are needed to
  # determine the global index of the event.

  if (length(results) != length(index.offsets))
    stop('ERROR: results and index.offsets must match.')

  events <- mapply(results, index.offsets, FUN = function(result, index.offset) {
    type.par.nr <- ifelse(attr(result, 'types') == 'TC', 2L, 1L)
    par.idx.delta <- cumsum(type.par.nr)[which(type.par.nr == 2L)]
    par.idx.alpha <- setdiff(seq.int(1L, length.out = sum(type.par.nr)), par.idx.delta)
    par.delta <- rep(NA, length(type.par.nr))
    par.delta[attr(result, 'types') == 'TC'] <- attr(result, 'par')[par.idx.delta]

    data.frame('type' = attr(result, 'types'),
               'index' = attr(result, 'indexes') + index.offset,
               'alpha' = attr(result, 'par')[par.idx.alpha],
               'delta' = par.delta)
  }, SIMPLIFY = FALSE)

  events <- data.table::rbindlist(events)
  if (nrow(events) == 0L) {
    events <- data.table::data.table('type' = character(), 'index' = integer(),
                                     'alpha' = numeric(), 'delta' = numeric())
  }
  structure(events, 'class' = c('Events', class(events)), 'n' = n)
}


#' @keywords internal
#'
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
               list('start' = ends[1L] - lengths[1L] + 1L,
                    'end' = pmin(ends[length(ends)] + nr.tail, nrow(dif))),
               by = traverse.id]

  results <- mapply(start = drle[, start], end = drle[, end], FUN = function(start, end) {
    df <- dif[start:end, ]
    seeker(x = df$vdiff, outlier = df$outlier, types = types, mu = df$mu, sigma2 = df$sigma2)
  }, SIMPLIFY = FALSE)

  events <- Events(results, drle[, start], n = length(x))
  if (nrow(events) != 0L) events[, index := idx[index]] # back to original idx

  set.version(events, Version('0.06'))

  events
}

