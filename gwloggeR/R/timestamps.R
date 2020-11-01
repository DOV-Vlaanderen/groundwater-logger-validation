#' @keywords internal
#'
round.timestamp <- function(timestamps, scalefactor.sec = 3600*12) {
  assert.timestamp(timestamps)
  as.POSIXct(round(as.numeric(timestamps)/scalefactor.sec) * scalefactor.sec, origin = '1970-01-01', tz = 'UTC')
}

#' @keywords internal
#'
is.timestamp <- function(timestamps) {
  inherits(timestamps, "POSIXct") | inherits(timestamps, "Date")
}

#' @keywords internal
#'
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

#' @keywords internal
#'
validate.hydropressure.timestamp <- function(timestamps, x) {
  if (is.null(timestamps)) stop('ERROR: for hydrostatic pressure one needs to supply timestamps.')
  if (length(timestamps) != length(x)) stop('ERROR: x and timestamps must have same length.')
  assert.timestamp(timestamps)
  assert.nonas(timestamps)
  assert.noduplicates(timestamps)
  assert.ordered(timestamps)
}

