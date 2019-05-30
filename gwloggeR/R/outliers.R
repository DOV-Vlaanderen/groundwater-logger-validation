#' @title Detect outliers
#' @description
#' This function marks outliers in the input vector.
#' @param x numeric vector of values
#' @return logical vector with same length as x, specifying TRUE for outlier
#' @examples
#' # In case of a vector:
#' x <- c(1:10, 100)
#' detect_outliers(x)
#'
#' # In case of a dataframe, select the column:
#' detect_outliers(cars$dist)
#'
#' # Or use the tidyverse approach:
#' library(magrittr)
#' cars %>% plyr::mutate("outlier" = detect_outliers(dist))
#'
#' @export
#' @rdname detect_outliers

detect_outliers <- function(x) {
  detect_outliers_leys(x)
}
