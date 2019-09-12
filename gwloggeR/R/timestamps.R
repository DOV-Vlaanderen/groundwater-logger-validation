#' @keywords internal
aggregate <- function(x, ts, by = c('days', 'hours')) {
  assert.timestamp(ts)
  by <- match.arg(by)

  stats::aggregate(x, by = list(as.POSIXct(trunc(ts, units = by))), FUN = mean)$x
}

is.timestamp <- function(timestamps) {
  inherits(timestamps, "POSIXct") | inherits(timestamps, "Date")
}

assert.timestamp <- function(timestamps) {
  if(!is.timestamp(timestamps)) stop('ERROR: timestamp must either be POSIXct or Date.')
}

assert.notna.timestamp <- function(timestamps) {
  if (any(is.na(timestamps))) stop('ERROR: timestamps may not be NA.')
}

validate.timestamp <- function(timestamps) {
  nr.na <- sum(is.na(timestamps))
  if (nr.na > 0L)
    warning(sprintf('%s out of %s timestamps are NA.',
                    nr.na,
                    length(timestamps)),
            call. = FALSE, immediate. = TRUE)
  nr.dupes <- sum(duplicated(timestamps[!is.na(timestamps)]))
  if (nr.dupes > 0L)
    warning(sprintf('%s out of %s timestamps are duplicates.',
                    nr.dupes,
                    length(timestamps)),
            call. = FALSE, immediate. = TRUE)

  is.timestamp(timestamps)
}
