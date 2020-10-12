#' @title Drift object
#' @description Drift object holds all the necessary information about the detected drift.
#' @keywords internal
#' @rdname Drift
#'
Drift <- function(x, ...) {
  UseMethod('Drift', x)
}

#' @rdname Drift
#'
Drift.logical <- function(x, mu, timestamp, rate, significance) {
  structure(x, 'class' = c('Drift', 'logical'),
            mu = mu,
            timestamp = timestamp, rate = rate, significance = significance)
}

#' @rdname Drift
#'
Drift.Arima <- function(model, timestamps) {
  stopifnot(!is.null(model$xreg))
  stopifnot('bptrend' %in% colnames(model$xreg))
  x <- rep(FALSE, length(timestamps))
  x[timestamps >= model$bp.ts] <- TRUE
  Drift(x, mu = model$coef[['intercept']],
        timestamp = model$bp.ts,
        significance = p.val(model)[['bptrend']],
        rate = structure(model$coef[['bptrend']], units = 'cmH2O/year'))
}


#' @title Detect drift
#' @description
#' This function marks a drift in the input vector.
#' @param x numeric vector of values
#' @param timestamps timestamp vector
#' @param reference a list of list(x, timestamps) pairs that are to be used as
#' reference for drift detection. The more data is supplied here, the better
#' the results. Note also that the barometer data supplied here should be
#' compensated for altitude.
#' @param apriori \link{Apriori} class. Defaults to air pressure in cmH2O.
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
  signature = c("x"),
  valueClass = "logical",
  function(x, timestamps, reference = list(),
           apriori = Apriori('air pressure', units = 'cmH2O'),
           ..., plot = FALSE, verbose = FALSE, title = NULL)
    standardGeneric('detect_drift')
)


#' Drift detection function
#' @importFrom methods setMethod
#' @describeIn detect_drift
#'
setMethod(
  'detect_drift',
  signature = c(x = "numeric"),
  function(x, timestamps, reference, apriori, plot, verbose, title) {

    if (apriori$data_type != "air pressure" && apriori$units != 'cmH2O')
      stop('Drift detection is only implemented for air pressure data in cmH2O.')

    xdr <- drift_reference.differentiate(x = x, timestamps = timestamps, reference = reference)

    model <- model_drifts.fit(x = xdr$x, timestamps = xdr$timestamps, ar1 = 0.9)

    drift <- Drift(model, timestamps)

    if (plot) plot_drifts(x = x, timestamps = timestamps, drift = drift, title = title)

    if (verbose) drift else as.vector(drift)
})

# detect_drift(rnorm(100), seq(Sys.time(), by = '12 hours', length.out = 100), verbose = TRUE, plot = TRUE)
# detect_drift(rnorm(100), seq(Sys.time(), by = '12 hours', length.out = 100), verbose = TRUE, plot = TRUE,
#              reference = list(list(x = rnorm(100), timestamps = seq(Sys.time(), by = '12 hours', length.out = 100))))
# detect_drift(x = gwloggeR::model_drifts.simulate(100, mu = 1032, sigma = 5, phi1 = 0.9),
#              timestamps = seq(Sys.time(), by = '12 hours', length.out = 100), verbose = TRUE, plot = TRUE,
#              reference = list(list(x = gwloggeR::model_drifts.simulate(100, mu = 1032, sigma = 5, phi1 = 0.9),
#                                    timestamps = seq(Sys.time(), by = '12 hours', length.out = 100))))
