#' @title Detect duplicate timestamps
#' @description
#' This function marks duplicate timestamps in the input vector.
#' @param ts timestamp vector of type Date or POSIXct
#' @return Logical vector with same length as x, specifying TRUE for a duplicate value.
#' @examples
#' # In case of a vector:
#' ts <- seq(as.Date('2019-01-01'), as.Date('2019-01-5'), by = 'day')
#' ts <- c(ts, as.Date('2019-01-5'))
#' detect_duplicates(ts)
#'
#' @export
#' @rdname detect_duplicates

detect_duplicates <- function(ts) {
  if(!(inherits(ts, "POSIXct") | inherits(ts, "Date")))
    stop('ERROR: ts must either be POSIXct or Date.')
  duplicated(ts)
}
