assert.timestamp <- function(timestamps) {
  if(!is.timestamp(timestamps)) stop('ERROR: timestamp must either be POSIXct or Date.')
}

assert.nonas <- function(timestamps) {
  if (any(is.na(timestamps))) stop('ERROR: timestamps may not be NA.')
}

assert.noduplicates <- function(timestamps) {
  if (sum(duplicated(timestamps)) > 0L) stop('ERROR: duplicate timestamps detected.')
}

assert.ordered <- function(timestamps) {
  if (!identical(sort(timestamps), timestamps)) stop('ERROR: timestamps must be ordered.')
}
