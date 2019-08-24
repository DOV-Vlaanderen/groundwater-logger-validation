#' @title Type I error rate constant
#' @keywords internal
CONST.ALPHA <- 1/2000


#' @title Outliers object
#' @description Outliers object holds all the necessary information about detected outiers.
#' @keywords internal

Outliers <- function(x.rejects, x.mean, x.sd,
                     sigma.reject, alpha, type,
                     fun.density, cutpoints) {
  structure(x.rejects, "class" = c("logical", "Outliers"),
            "x.mean" = x.mean, "x.sd" = x.sd,
            "sigma.reject" = sigma.reject, "alpha" = alpha, "type" = type,
            "fun.density" = fun.density, "cutpoints" = cutpoints)
}


#' @title Detect outliers
#' @description
#' This function marks outliers in the input vector.
#' @param x numeric vector of values
#' @param apriori \link{apriori} class
#' @param ... optional parameters, depending on signature:
#' @param plot prints comprehensive plots
#' @param verbose prints comprehensive information
#' @param title adds title to the plot
#' @param timestamps timestamp vector. Has no direct meaning, except aestehtical
#' for the scatter plot if plot = TRUE. In case there are duplicates and NA
#' values, only warnings will be raised which might suggest that something is
#' wrong with x.
#' @return Logical vector with same length as x, specifying TRUE for an outlier.
#' @examples
#' # In case of a vector:
#' x <- c(1:10, 100)
#' detect_outliers(x)
#'
#' # In case of a dataframe, select the column:
#' detect_outliers(cars$dist)
#'
#' \dontrun{
#' # Or use the tidyverse approach:
#' library(magrittr)
#' cars %>% dplyr::mutate("outlier" = detect_outliers(dist))
#' }
#'
#' @export
#' @rdname detect_outliers

setGeneric("detect_outliers",
           signature = c("x", "apriori"),
           valueClass = "logical",
           function(x, apriori, ..., plot = FALSE, verbose = FALSE, title = NULL, timestamps = NULL)
             standardGeneric('detect_outliers')
)

#' @describeIn detect_outliers
#' Only considers x, without any _a-priori_ information.
#' A normal distribution is assumed with mean and variance estimated using
#' median and MAD as described in Leys, 2013.
#' @references Leys, C. e.a., Detecting outliers, 2013.

setMethod(
  'detect_outliers',
  signature = c(x = "numeric", apriori = "missing"),
  function(x, plot, verbose, title, timestamps) {

    x.mean <- median(x, na.rm = TRUE)
    x.sd <- mad(x, na.rm = TRUE)
    outliers <- detect_outliers_norm(x, x.mean = x.mean, x.sd = x.sd)

    if (!is.null(timestamps)) {
      assert.timestamp(timestamps)
      validate.timestamp(timestamps)
      if (length(timestamps) != length(x)) stop('x and timestamps must have same length.')
    }

    if (plot) outliers_plot(x = x, outliers = outliers, timestamps = timestamps, title = title)

    if (verbose) outliers else as.vector(outliers)
})

#' @describeIn detect_outliers
#' Takes _a-priori_ information about x into consideration.

setMethod(
  'detect_outliers',
  signature = c(x = "numeric", apriori = "apriori"),
  function(x, apriori, plot, verbose, title, timestamps) {

    if (!is.null(timestamps)) {
      assert.timestamp(timestamps)
      validate.timestamp(timestamps)
      if (length(timestamps) != length(x)) stop('x and timestamps must have same length.')
    }

    if (apriori$data_type == "air pressure") return ({
      outliers <- detect_outliers_norm(x, x.mean = apriori$mean, x.sd = sqrt(apriori$var))
      if (plot) outliers_plot(x = x, outliers = outliers, timestamps = timestamps, show.qqplot = FALSE, title = title)
      if (verbose) outliers else as.vector(outliers)
    })

    if (apriori$data_type == "hydrostatic pressure") return ({
      det <- detect(x = x, timestamps = timestamps)
      rejects.x <- rep(FALSE, length(x))
      rejects.x[det[type == 'AO', index]] <- TRUE

      outliers <- Outliers(rejects.x, x.mean = NULL, x.sd = NULL, alpha = NULL, sigma.reject = NULL,
                           type = "two.sided", fun.density = NULL, cutpoints = NULL)

      if (plot) {
        d <- differenceplot(x, timestamps = timestamps)
        s <- scatterplot(x, outliers = outliers, timestamps = timestamps)
        layout_matrix <- rbind(c(1,1),
                               c(2,2))
        grob.title <- if (!is.null(title)) grid::textGrob(title, x = 0.05, hjust = 0)
        gridExtra::grid.arrange(s, d, layout_matrix = layout_matrix,
                                top = grob.title)
      }

      if (verbose) outliers else as.vector(outliers)
    })

    NULL
})
