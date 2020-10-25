#' @title A-priori data information class
#' @description
#' This function creates an Apriori class based on input.
#' @param data_type character, specifying type of data
#' @param units character, specifying units of data
#' @return Apriori object.
#' @examples
#' # For example:
#' ap <- Apriori("air pressure", "cmH2O")
#'
#' @export
#'
Apriori <- function(data_type = c("air pressure", "hydrostatic pressure"), units = c("cmH2O")) {
  data_type <- match.arg(data_type)
  units <- match.arg(units)

  structure(
    list("mean" = 1033.317,
         "var" = 9.586297^2,
         "data_type" = data_type,
         "units" = units),
    class = "Apriori")


}

setOldClass('Apriori') # for use in S4 slots and signatures

#' @rdname Apriori
#' @export
#'
apriori <- function(data_type = c("air pressure", "hydrostatic pressure"), units = c("cmH2O")) {
  warning('apriori() function is deprecated: please rename it to Apriori().', call. = FALSE, immediate. = TRUE)
  Apriori(data_type = data_type, units = units)
}
