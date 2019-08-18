#' @importFrom data.table :=
#' @keywords internal
apriori.hydropressure.difference.samples <- function(interval.sec) {
  # Unique time differences (DIFF.SEC) per FILE.
  df.map <- hydropressure[, .(DIFF.SEC = unique(as.numeric(diff(TIMESTAMP_UTC), units = 'secs'))),
                          by = FILE]

  # Make sure that DIFF.SEC is unique per FILE.
  if (length(unique(df.map[, FILE])) != nrow(df.map))
    stop('ERROR: hydropressure time differences not unique per file.')

  # You can not use e.g. 15 min frequency data to simulate 20 min data.
  # You can not use e.g. 15 min frequency data to simulate 5 min data.
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

detect <- function(x, timestamps) {
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
                       'c' = c.norm.optimal(alpha = 1/2000, n = n, type = 'two.sided'))
  }, SIMPLIFY = FALSE)
  map <- data.table::rbindlist(map)
  dif <- data.table::data.table('tsdiff' = tsdiff, 'vdiff' = vdiff)
  dif <- merge(dif, map, all.x = TRUE)
  dif[, 'outlier' := vdiff < lower.bound*1.5 | vdiff > upper.bound*1.5]

  drle <- data.table::as.data.table(unclass(rle(dif$outlier)))
  drle[, 'cumsum' := cumsum(lengths)]
  drle[(values), 'start' := cumsum - lengths]
  drle[(values), 'end' := cumsum + 1L]
  drle[(values)]

  res <- mapply(start = drle[(values), start], end = drle[(values), end], FUN = function(start, end) {
    res <- seeker2(x = dif[start:end, vdiff], mu = dif[start:end, mu], sigma2 = dif[start:end, sigma2])
    res <- res[[length(res)]]

    dLS <- if (is.null(attr(res, 'ls_index_vec'))) data.frame() else data.frame(type = 'LS', 'index' = attr(res, 'ls_index_vec') + start)
    dAO <- if (is.null(attr(res, 'ao_index_vec'))) data.frame() else data.frame(type = 'AO', 'index' = attr(res, 'ao_index_vec') + start)
    data.table::rbindlist(list(dLS, dAO))
  }, SIMPLIFY = FALSE)

  data.table::rbindlist(res)
}

