#' @title Temporalchanges object
#' @description Temporalchanges object holds all the necessary information about detected temporal changes.
#' @keywords internal
#'
Temporalchanges <- function(vec) {
  if (!is.logical(vec)) stop('ERROR: input vector must be a logical.')
  structure(vec, 'class' = c('logical', 'Temporalchanges'))
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

      det <- detect(x = x, timestamps = timestamps)
      ls.x <- rep(FALSE, length(x))
      ls.x[det[type == 'TC', index]] <- TRUE

      temporalchanges <- Temporalchanges(ls.x)
      set.version(temporalchanges, attr(det, 'version'))

      if (verbose) temporalchanges else as.vector(temporalchanges)
    })

    NULL
  })
