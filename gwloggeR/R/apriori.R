#' @title A-priori data information class
#' @description
#' This function creates an a-priori class based on input.
#' @param data_type character, specifying type of data
#' @param units character, specifying units of data
#' @return A-priori object.
#' @examples
#' # For example:
#' ap <- apriori("air pressure", "cmH2O")
#'
#' @export
#'
apriori <- function(data_type = c("air pressure"), units = c("cmH2O")) {
  data_type <- match.arg(data_type)
  units <- match.arg(units)

  structure(
    list(mean = 1034.2,
         var = 98.67283),
    class = 'apriori')
}

setOldClass('apriori')