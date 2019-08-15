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
