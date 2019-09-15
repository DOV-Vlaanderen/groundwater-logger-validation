#' @title Levelshifts object
#' @description Levelshifts object holds all the necessary information about detected levelshifts.
#' @keywords internal
#'
Levelshifts <- function(vec) {
  if (!is.logical(vec)) stop('ERROR: input vector must be a logical.')
  structure(vec, 'class' = c('logical', 'Levelshifts'))
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

      hydropressure.timestamp.validation(timestamps, x)

      det <- detect(x = x, timestamps = timestamps)
      ls.x <- rep(FALSE, length(x))
      ls.x[det[type == 'LS', index]] <- TRUE

      levelshifts <- Levelshifts(ls.x)
      set.version(levelshifts, attr(det, 'version'))

      if (plot) plot.generic(x = x, timestamps = timestamps, df.types = det, title = title)

      if (verbose) levelshifts else as.vector(levelshifts)
    })

    NULL
})
