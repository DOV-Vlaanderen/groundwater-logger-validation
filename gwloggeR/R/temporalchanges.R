#' @title Temporalchanges object
#' @description Temporalchanges object holds all the necessary information about detected temporal changes.
#' @keywords internal
#' @rdname Temporalchanges
#'
Temporalchanges <- function(x) {
  UseMethod('Temporalchanges', x)
}


#' @rdname Temporalchanges
#'
Temporalchanges.logical <- function(x) {
  structure(x, 'class' = c('Temporalchanges', 'logical'))
}


#' @rdname Temporalchanges
#'
Temporalchanges.Events <- function(events) {
  x <- rep(FALSE, attr(events, 'n'))
  x[events[type == 'TC', index]] <- TRUE
  set.version(x, attr(events, 'version'))
  Temporalchanges(x)
}


#' @title Detect Temporal changes
#' @description
#' This function marks temporalchanges in the input vector.
#' @param x numeric vector of values
#' @param timestamps timestamp vector
#' @param apriori \link{apriori} class
#' @param ... optional parameters, depending on signature:
#' @param plot prints comprehensive plots
#' @param verbose prints comprehensive information
#' @param title adds title to the plot
#' @return Logical vector with same length as x, specifying TRUE for the duration of a temporal change.
#' @importFrom methods setGeneric
#' @export
#' @rdname detect_temporalchanges
#'
setGeneric(
  "detect_temporalchanges",
  signature = c("x", "apriori"),
  valueClass = "logical",
  function(x, timestamps, apriori, ..., plot = FALSE, verbose = FALSE, title = NULL)
    standardGeneric('detect_temporalchanges')
)


#' Temporalchange detection function
#' @importFrom methods setMethod
#' @describeIn detect_temporalchanges
#'
setMethod(
  'detect_temporalchanges',
  signature = c(x = "numeric", apriori = "apriori"),
  function(x, timestamps, apriori, plot, verbose, title) {

    if (apriori$data_type == "air pressure") return ({
      warning('WARNING: temporal changes for air pressure data not implemented.')
      Temporalchanges(rep(FALSE, length(x)))
    })

    if (apriori$data_type == "hydrostatic pressure") return ({

      hydropressure.timestamp.validation(timestamps, x)

      events <- detect(x = x, timestamps = timestamps)
      temporalchanges <- Temporalchanges(events)

      if (plot) plot.generic(x = x, timestamps = timestamps, events = events, title = title)

      if (verbose) temporalchanges else as.vector(temporalchanges)
    })

    NULL
})
