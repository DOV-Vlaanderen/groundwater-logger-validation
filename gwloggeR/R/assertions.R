#' @keywords internal
assert.timestamp <- function(timestamps) {
  if(!is.timestamp(timestamps)) stop('ERROR: timestamp must either be POSIXct or Date.')
}

#' @keywords internal
assert.nonas <- function(timestamps) {
  if (any(is.na(timestamps))) stop('ERROR: timestamps may not be NA.')
}

#' @keywords internal
assert.noduplicates <- function(timestamps) {
  if (sum(duplicated(timestamps)) > 0L) stop('ERROR: duplicate timestamps detected.')
}

#' @keywords internal
assert.ordered <- function(timestamps) {
  if (!identical(sort(timestamps), timestamps)) stop('ERROR: timestamps must be ordered.')
}
