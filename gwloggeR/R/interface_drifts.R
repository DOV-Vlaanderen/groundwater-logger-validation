#' @title Drift object
#' @description Drift object holds all the necessary information about the detected drift.
#' @keywords internal
#' @rdname Drift
#'
Drift <- function(x) {
  UseMethod('Drift', x)
}

#' @rdname Drift
#'
Drift.logical <- function(x) {
  structure(x, 'class' = c('Drift', 'logical'))
}


#' @title Detect drift
#' @description
#' This function marks a drift in the input vector.
#' @param x numeric vector of values
#' @param timestamps timestamp vector
#' @param apriori \link{apriori} class. Defaults to air pressure.
#' @param ... optional parameters, depending on signature:
#' @param plot prints comprehensive plots
#' @param verbose prints comprehensive information
#' @param title adds title to the plot
#' @return Logical vector with same length as x, specifying TRUE for drifting observations.
#' @references
#' \href{https://dov-vlaanderen.github.io/groundwater-logger-validation/gwloggeR/docs/articles/Airpressure.html}{Air pressure vignette}
#' @importFrom methods setGeneric
#' @export
#' @rdname detect_drift
#'
setGeneric(
  "detect_drift",
  signature = c("x", "apriori"),
  valueClass = "logical",
  function(x, timestamps, apriori = apriori('air pressure', units = 'cmH2O'), ..., plot = FALSE, verbose = FALSE, title = NULL)
    standardGeneric('detect_drift')
)


#' Drift detection function
#' @importFrom methods setMethod
#' @describeIn detect_drift
#'
setMethod(
  'detect_drift',
  signature = c(x = "numeric", apriori = "apriori"),
  function(x, timestamps, apriori, plot, verbose, title) {

    if (apriori$data_type != "air pressure" && apriori$units != 'cmH2O') return ({
      warning('WARNING: drift detection is only implemented for air pressure in cmH2O.')
      return(Drift(rep(FALSE, length(x))))
    })

    validate.hydropressure.timestamp(timestamps, x)

    events <- detect(x = x, timestamps = timestamps)

    drift <- Drift(events)

    if (plot) plot_diagnostics_drift(x = x, timestamps = timestamps, events = events, title = title)

    if (verbose) drift else as.vector(drift)
})
