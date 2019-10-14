#' @title Detect duplicate timestamps
#' @description
#' This function marks duplicate timestamps in the input vector.
#' @param timestamps timestamp vector of type Date or POSIXct
#' @return Logical vector with same length as x, specifying TRUE for a duplicate value.
#' @examples
#' # In case of a vector:
#' ts <- seq(as.Date('2019-01-01'), as.Date('2019-01-5'), by = 'day')
#' ts <- c(ts, as.Date('2019-01-5'))
#' detect_duplicates(ts)
#'
#' @export
#' @rdname detect_duplicates

detect_duplicates <- function(timestamps) {
  assert.timestamp(timestamps)
  duplicated(timestamps)
}
