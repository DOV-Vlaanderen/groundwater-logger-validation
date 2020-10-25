#' @keywords internal
assert.timestamp <- function(timestamps) {
  if(!is.timestamp(timestamps)) stop('ERROR: timestamp must either be POSIXct or Date.')
}

#' @keywords internal
assert.nonas <- function(x) {
  if (any(is.na(x))) stop(sprintf('%s may not contain NA values.', deparse(substitute(x))), call. = FALSE)
}

#' @keywords internal
assert.noduplicates <- function(x) {
  if (sum(duplicated(x)) > 0L) stop(sprintf('%s may not contain duplicate values.', deparse(substitute(x))), call. = FALSE)
}

#' @keywords internal
assert.ordered <- function(x) {
  if (!identical(sort(x), x)) stop(sprintf('%s must be ordered.', deparse(substitute(x))), call. = FALSE)
}
