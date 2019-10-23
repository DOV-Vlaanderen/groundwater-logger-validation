#' @title Levelshifts object
#' @description Levelshifts object holds all the necessary information about detected levelshifts.
#' @rdname Levelshifts
#' @keywords internal
#' @rdname Levelshifts
#'
Levelshifts <- function(x) {
  UseMethod('Levelshifts', x)
}


#' @rdname Levelshifts
#'
Levelshifts.logical <- function(x) {
  structure(x, 'class' = c('Levelshifts', 'logical'))
}


#' @rdname Levelshifts
#'
Levelshifts.Events <- function(events) {
  x <- rep(FALSE, attr(events, 'n'))
  x[events[type == 'LS', index]] <- TRUE
  set.version(x, attr(events, 'version'))
  Levelshifts(x)
}

#' @title Detect levelshifts
#' @description
#' This function marks levelshifts in the input vector.
#' @param x numeric vector of values
#' @param timestamps timestamp vector
#' @param apriori \link{apriori} class
#' @param ... optional parameters, depending on signature:
#' @param plot prints comprehensive plots
#' @param verbose prints comprehensive information
#' @param title adds title to the plot
#' @return Logical vector with same length as x, specifying TRUE for an levelshift start.
#' @references
#' \href{https://dov-vlaanderen.github.io/groundwater-logger-validation/gwloggeR/docs/articles/Hydropressure.html}{Hydrostatic pressure vignette}
#' @importFrom methods setGeneric
#' @export
#' @rdname detect_levelshifts
#'
setGeneric(
  "detect_levelshifts",
  signature = c("x", "apriori"),
  valueClass = "logical",
  function(x, timestamps, apriori, ..., plot = FALSE, verbose = FALSE, title = NULL)
    standardGeneric('detect_levelshifts')
)


#' Levelshift detection function
#' @importFrom methods setMethod
#' @describeIn detect_levelshifts
#'
setMethod(
  'detect_levelshifts',
  signature = c(x = "numeric", apriori = "apriori"),
  function(x, timestamps, apriori, plot, verbose, title) {

    if (apriori$data_type == "air pressure") return ({
      warning('WARNING: levelshifts for air pressure data not implemented.')
      Levelshifts(rep(FALSE, length(x)))
    })

    if (apriori$data_type == "hydrostatic pressure") return ({

      validate.hydropressure.timestamp(timestamps, x)

      events <- detect(x = x, timestamps = timestamps)
      levelshifts <- Levelshifts(events)

      if (plot) plot.generic(x = x, timestamps = timestamps, events = events, title = title)

      if (verbose) levelshifts else as.vector(levelshifts)
    })

    NULL
})
