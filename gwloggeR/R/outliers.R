#' @title Detect outliers
#' @description
#' This function marks outliers in the input vector.
#' @param x numeric vector of values
#' @param apriori \link{apriori} class
#' @param plot prints comprehensive plots
#' @param verbose prints comprehensive information
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
           function(x, apriori, plot = FALSE, verbose = FALSE) standardGeneric('detect_outliers')
)

#' @describeIn detect_outliers
#' Only considers x, without any _a-priori_ information.
#' A normal distribution is assumed with mean and variance estimated using
#' median and MAD as described in Leys, 2013.
#' @references Leys, C. e.a., Detecting outliers, 2013.

setMethod(
  'detect_outliers',
  signature = c(x = "numeric", apriori = "missing"),
  function(x, plot = FALSE, verbose = FALSE) {

    x.mean <- median(x, na.rm = TRUE)
    x.sd <- mad(x, na.rm = TRUE)
    outliers <- detect_outliers_norm(x, x.mean = x.mean, x.sd = x.sd)

    if (plot) outlierplot(x = x, outliers = outliers)
    if (verbose) cat(outliers_verbose(outliers))

    as.vector(outliers)
})

#' @describeIn detect_outliers
#' Takes _a-priori_ data information into consideration.

setMethod('detect_outliers',
          signature = c(x = "numeric", apriori = "apriori"),
          function(x, apriori) {

  if (apriori$data_type == "air pressure") return (
    detect_outliers_norm(x, x.mean = apriori$mean, x.sd = sqrt(apriori$var))
  )

  if (apriori$data_type == "hydrostatic pressure") return ({
    # It is very unlikely that hydrostatic pressure pressure is lower than a-priori air pressure.
    l <- detect_outliers_norm(x, x.mean = apriori$mean,
                              x.sd = sqrt(apriori$var), type = "less")
    # We use the detect_outliers(x) method on remaining data points.
    r <- detect_outliers_norm(x, x.mean = median(x[!l], na.rm = TRUE),
                              x.sd = mad(x[!l], na.rm = TRUE), type = "greater")
    l | r
  })

  NULL
})
