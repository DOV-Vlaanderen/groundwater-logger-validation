#' @keywords internal
assert.timestamp <- function(x) {
  name <- deparse(substitute(x))
  if(!is.timestamp(x)) stop(sprintf('Variable %s must either be POSIXct or Date.', name), call. = FALSE)
}

#' @keywords internal
assert.nonas <- function(x) {
  name <- deparse(substitute(x))
  if (any(is.na(x))) stop(sprintf('Variable %s may not contain NA values.', name), call. = FALSE)
}

#' @keywords internal
assert.noduplicates <- function(x, warn.only = FALSE) {
  has.duplicates <- sum(duplicated(x)) > 0L
  name <- deparse(substitute(x))
  if (has.duplicates) {
    if (warn.only)
      warning(sprintf('Variable %s has duplicate values.', name), call. = FALSE, immediate. = TRUE)
    else
      stop(sprintf('Variable %s may not contain duplicate values.', name), call. = FALSE)
  }
}

#' @keywords internal
assert.ordered <- function(x) {
  name <- deparse(substitute(x))
  if (!identical(sort(x), x)) stop(sprintf('Variable %s must be ordered.', name), call. = FALSE)
}

#' @keywords internal
assert.numeric <- function(x) {
  name <- deparse(substitute(x))
  if (!inherits(x, 'numeric')) stop(sprintf('Variable %s must be of class numeric.', name), call. = FALSE)
}
