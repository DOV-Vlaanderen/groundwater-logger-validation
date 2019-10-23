#' @title Type I error rate constant
#' @keywords internal
CONST.ALPHA <- 1/2000


#' @title Outliers object
#' @description Outliers object holds all the necessary information about detected outiers.
#' @keywords internal
#' @rdname Outliers
#'
Outliers <- function(x, ...) {
  UseMethod('Outliers', x)
}


#' @rdname Outliers
#'
Outliers.logical <- function(x.rejects, x.mean, x.sd,
                             sigma.reject, alpha, type,
                             fun.density, cutpoints) {
  structure(x.rejects, "class" = c("Outliers", "logical"),
            "x.mean" = x.mean, "x.sd" = x.sd,
            "sigma.reject" = sigma.reject, "alpha" = alpha, "type" = type,
            "fun.density" = fun.density, "cutpoints" = cutpoints)
}


#' @rdname Outliers
#'
Outliers.Events <- function(events) {
  x <- rep(FALSE, attr(events, 'n'))
  x[events[type == 'AO', index]] <- TRUE
  set.version(x, attr(events, 'version'))
  Outliers(x, x.mean = NULL, x.sd = NULL, alpha = NULL, sigma.reject = NULL,
           type = "two.sided", fun.density = NULL, cutpoints = NULL)
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
#' @param timestamps timestamp vector. For airpressure, timestamps are of
#' no importance, except aestehtical for the scatter plot if plot = TRUE.
#' In case there are duplicates and NA values, only warnings will be raised
#' which might suggest that something is wrong with x.
#' For hydrostatic pressure, timestamps are important. Therefore an error
#' is raised if timestamps are not supplied, or if any of the timestamps
#' are NA or duplicates.
#' @return Logical vector with same length as x, specifying TRUE for an outlier.
#' @references
#' \href{https://dov-vlaanderen.github.io/groundwater-logger-validation/gwloggeR/docs/articles/Airpressure.html}{Air pressure vignette}
#' \href{https://dov-vlaanderen.github.io/groundwater-logger-validation/gwloggeR/docs/articles/Hydropressure.html}{Hydrostatic pressure vignette}
#' @examples
#' # In case of a vector:
#' x <- c(1:9, 100)
#' detect_outliers(x)
#'
#' # In case of a dataframe, select the column:
#' df <- data.frame('x' = x)
#' detect_outliers(df$x)
#'
#' # Or use the tidyverse approach:
#' library(magrittr)
#' df %>% dplyr::mutate("outlier" = detect_outliers(x))
#'
#' @importFrom methods setGeneric
#' @export
#' @rdname detect_outliers
#'
setGeneric(
  "detect_outliers",
  signature = c("x", "apriori"),
  valueClass = "logical",
  function(x, apriori, ..., plot = FALSE, verbose = FALSE, title = NULL, timestamps = NULL)
    standardGeneric('detect_outliers')
)

#' @describeIn detect_outliers
#' Only considers x, without any _a-priori_ information.
#' A normal distribution is assumed with mean and variance estimated using
#' median and MAD as described in Leys, 2013.
#' @importFrom methods setMethod
#'
setMethod(
  'detect_outliers',
  signature = c(x = "numeric", apriori = "missing"),
  function(x, plot, verbose, title, timestamps) {

    x.mean <- median(x, na.rm = TRUE)
    x.sd <- mad(x, na.rm = TRUE)
    outliers <- detect_outliers_norm(x, x.mean = x.mean, x.sd = x.sd)
    set.version(outliers, Version('0.01'))

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
#' @importFrom methods setMethod
#'
setMethod(
  'detect_outliers',
  signature = c(x = "numeric", apriori = "apriori"),
  function(x, apriori, plot, verbose, title, timestamps) {

    if (apriori$data_type == "air pressure") return ({

      # timestamp checks
      if (!is.null(timestamps)) {
        assert.timestamp(timestamps)
        validate.timestamp(timestamps)
        if (length(timestamps) != length(x)) stop('ERROR: x and timestamps must have same length.')
      }

      outliers <- detect_outliers_norm(x, x.mean = apriori$mean, x.sd = sqrt(apriori$var))
      set.version(outliers, Version('0.03'))

      if (plot) outliers_plot(x = x, outliers = outliers, timestamps = timestamps, show.qqplot = FALSE, title = title)
      if (verbose) outliers else as.vector(outliers)
    })

    if (apriori$data_type == "hydrostatic pressure") return ({

      validate.hydropressure.timestamp(timestamps, x)

      events <- detect(x = x, timestamps = timestamps)
      outliers <- Outliers(events)

      if (plot) plot.generic(x = x, timestamps = timestamps, events = events, title = title)

      if (verbose) outliers else as.vector(outliers)
    })

    NULL
})
