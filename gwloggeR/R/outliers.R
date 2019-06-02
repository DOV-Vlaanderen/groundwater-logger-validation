#' @title Detect outliers
#' @description
#' This function marks outliers in the input vector.
#' @param x numeric vector of values
#' @return Logical vector with same length as x, specifying TRUE for an outlier.
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
#' cars %>% dplyr::mutate("outlier" = detect_outliers(dist))
#'
#' @export
#' @rdname detect_outliers
setGeneric("detect_outliers",
           signature = c("x", "apriori"),
           valueClass = "logical",
           function(x, apriori) standardGeneric('detect_outliers')
)

#' @describeIn detect_outliers
#' Only considers x, without any _a-priori_ information.
#' A normal distribution is assumed with mean and variance estimated using
#' median and MAD as described in Leys, 2013.
#' @references Leys, C. e.a., Detecting outliers, 2013.
setMethod('detect_outliers',
          signature = c(x = "numeric", apriori = "missing"),
          function(x) {
  detect_outliers_norm(x, x.mean = median(x, na.rm = TRUE), x.sd = mad(x, na.rm = TRUE))
})

#' @describeIn detect_outliers
#' Takes _a-priori_ data information into consideration.
setMethod('detect_outliers',
          signature = c(x = "numeric", apriori = "apriori"),
          function(x, apriori) {
  detect_outliers_norm(x, x.mean = apriori$mean, x.sd = sqrt(apriori$var))
})
