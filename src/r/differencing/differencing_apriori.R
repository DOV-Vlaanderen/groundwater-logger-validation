df.list <- sapply(Logger::enumerate('geotech'), function(name) Logger(name)$df, simplify = FALSE, USE.NAMES = TRUE)

differentiate <- function(df, interval.sec, get_possible_intervals = FALSE, diag_plots = FALSE) {
  df <- data.table::copy(df)
  if (any(is.na(df$TIMESTAMP_UTC))) stop('ERROR: all timestamps must be non-NA.')
  data.table::setkey(df, TIMESTAMP_UTC)

  possibilities.sec <- as.integer(difftime(df$TIMESTAMP_UTC, min(df$TIMESTAMP_UTC), units = 'secs'))
  possibility.sec.min <- min(possibilities.sec)

  # select records with the required time interval
  sel <- possibilities.sec %% interval.sec == 0
  df2 <- df[sel, ]

  diffs <- diff(df2$PRESSURE_VALUE)

  # take differences
  if (diag_plots) {
    hist(diffs, xlim = quantile(diffs, probs = c(0.001, 0.999)), breaks = 10000)
    hist(abs(diffs), xlim = quantile(abs(diffs), probs = c(0.001, 0.999)), breaks = 10000)
  }

  diffs
}

differentiate(Logger(Logger::enumerate('geotech')[1])$df, interval.sec = 1800, diag_plots = TRUE)

df <- Logger(Logger::enumerate('geotech')[1])$df


# return difference without timestamp if it is possible to make
# one based on interval
# it returns either a value or NA
diffx <- function(values, timestamps, interval.sec) {
  n <- length(timestamps)
  if (n <= 1L) return(as.numeric(NA))

  ordered.indexes <- order(timestamps)
  values <- values[ordered.indexes]; timestamps <- timestamps[ordered.indexes]

  max.diff <- as.integer(difftime(timestamps[n], timestamps[1], units = 'secs'))
  if (interval.sec > max.diff) return(as.numeric(NA))

  # if possible to form a diff, then find one
  for (start in 1L:(n-1L)) {
    tdiffs <- as.integer(difftime(timestamps[-(1L:start)], timestamps[start], units = 'secs'))
    hit.index <- which(tdiffs == interval.sec) + start
    if (length(hit.index) > 1L) stop('ERROR: more than 1 hit, maybee ')
    return(values[hit.index] - values[start])
  }

  return(as.numeric(NA))
}

diffx(df[1:3, PRESSURE_VALUE], df[1:3, TIMESTAMP_UTC], interval.sec = 1800)

diffx2 <- function(values, timestamps, interval.sec) {
  maxts.index <- which.max(timestamps)
  tdiffs <- as.integer(difftime(timestamps[maxts.index], timestamps, units = 'secs'))
  hit.index <- which(tdiffs == interval.sec)
  if (length(hit.index) > 1L) stop('ERROR: more than 1 hit, maybee ')
  if (length(hit.index) == 0L) return(as.numeric(NA))
  values[maxts.index] - values[hit.index]
}

differentiate2 = function(df, interval.sec) {
  df <- data.table::copy(df)
  if (any(is.na(df$TIMESTAMP_UTC))) stop('ERROR: all timestamps must be non-NA.')
  data.table::setkey(df, TIMESTAMP_UTC)

  values <- df$PRESSURE_VALUE
  timestamps <- df$TIMESTAMP_UTC
  tdiffs <- as.integer(difftime(df[, TIMESTAMP_UTC], df[1L, TIMESTAMP_UTC], units = 'secs'))

  i <- sample(which(tdiffs < interval.sec), 1L) # Or simply 1L
  N <- length(values)
  diffs <- NULL

  while (i <= N) {
    e <- i + 1L
    while (e <= N) {
      d <- diffx2(values[i:e], timestamps[i:e], interval.sec = interval.sec)
      if (!is.na(d)) {
        diffs[length(diffs) + 1L] <- d
        break
      } else {
        e <- e + 1L
      }
    }
    i <- e
  }

  diffs
}

plot.hist <- function(diffs) {
  hist(abs(diffs), xlim = quantile(abs(diffs), probs = c(0.001, 0.999)), breaks = 10000)
  invisible()
}

tmp <- differentiate(Logger(Logger::enumerate('geotech')[1])$df, interval.sec = 2700)
tmp2 <- differentiate2(Logger(Logger::enumerate('geotech')[1])$df, interval.sec = 2700)
identical(tmp, tmp2)

tdiffs.sec <- c(5, 10, 15, 30, 45, 60, 120, 240, 360, 720, 1080, 1440, 2880)*60 # sec
#set.seed(2019)
#tdiffs.sec <- sample(seq(5, 2880, by = 5), 200)*60

df.diffs <- list()
df.diffs[[as.character(30*60)]] <- sapply(df.list, FUN = differentiate2, interval.sec = 30*60, simplify = FALSE, USE.NAMES = TRUE)
df.diffs[[as.character(5*60)]] <- sapply(df.list, FUN = differentiate2, interval.sec = 5*60, simplify = FALSE, USE.NAMES = TRUE)
df.diffs[[as.character(1440*60)]] <- sapply(df.list, FUN = differentiate2, interval.sec = 1440*60, simplify = FALSE, USE.NAMES = TRUE)

sapply(df.diffs[['300']], function(diffs) if (!is.null(diffs)) plot.hist(diffs))

tmp3 <- unlist(unname(df.diffs[['86400']]))
plot.hist(tmp3)


local({
  print(Sys.time())
  cl <- parallel::makeCluster(7, outfile = './log/parallel.log')
  on.exit(parallel::stopCluster(cl))

  parallel::clusterExport(cl = cl, varlist = c('differentiate2', 'diffx2', 'df.list'))

  df.diffs <<- parallel::clusterApplyLB(cl = cl, x = tdiffs.sec, fun = function(interval.sec) {
    sapply(df.list, FUN = differentiate2, interval.sec = interval.sec, simplify = FALSE, USE.NAMES = TRUE)
  })
  names(df.diffs) <<- tdiffs.sec

  print(Sys.time())
})

df <- data.table::rbindlist(lapply(names(df.diffs), function(tdiff.name) {
  data.table::data.table('TIMEDIFF' = as.numeric(tdiff.name),
                         'VALUE' = unlist(unname(df.diffs[[tdiff.name]])))
}), use.names = TRUE, idcol = FALSE)

ggplot2::ggplot(data = df[, .(MEDIAN = median(abs(VALUE)),
                              N = length(VALUE),
                              Q.025 = quantile(abs(VALUE), 0.025),
                              Q.975 = quantile(abs(VALUE), 0.975),
                              Q.99 = quantile(abs(VALUE), 0.99)),
                          by = TIMEDIFF/60/60],
                mapping = ggplot2::aes(x = TIMEDIFF)) +
  ggplot2::geom_line(mapping = ggplot2::aes(y = MEDIAN)) +
  ggplot2::geom_line(mapping = ggplot2::aes(y = Q.975), col = 'red') +
  ggplot2::ylab('VALUEDIFF')


names(df.diffs)
saveRDS(df.diffs, './differencing/df.diffs.rds')
