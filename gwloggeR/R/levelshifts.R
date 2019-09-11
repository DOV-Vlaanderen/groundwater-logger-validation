#' @title Levelshifts object
#' @description Levelshifts object holds all the necessary information about detected levelshifts.
#' @keywords internal

Levelshifts <- function(vec) {
  structure(vec)
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

    if (!is.null(timestamps)) {
      assert.timestamp(timestamps)
      validate.timestamp(timestamps)
      if (length(timestamps) != length(x)) stop('x and timestamps must have same length.')
    }

    if (apriori$data_type == "hydrostatic pressure") return ({
      det <- detect(x = x, timestamps = timestamps)
      ls.x <- rep(FALSE, length(x))
      ls.x[det[type == 'LS', index]] <- TRUE

      levelshifts <- Levelshifts(ls.x)

      if (plot) {
        d <- differenceplot(x, timestamps = timestamps, outliers = levelshifts)
        s <- scatterplot.levelshifts(x, levelshifts = levelshifts, timestamps = timestamps)
        layout_matrix <- rbind(c(1,1),
                               c(2,2))
        grob.title <- if (!is.null(title)) grid::textGrob(title, x = 0.05, hjust = 0)
        gridExtra::grid.arrange(s, d, layout_matrix = layout_matrix,
                                top = grob.title)
      }

      if (verbose) levelshifts else as.vector(levelshifts)
    })

    NULL
})
