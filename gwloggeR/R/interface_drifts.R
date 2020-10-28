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
Drift.logical <- function(x, mu, sigma, timestamp = as.POSIXct(NA), rate = as.numeric(NA),
                          year.seasonality = c('sine' = as.numeric(NA), 'cosine' = NA),
                          significance = as.numeric(NA)) {

  structure(x, 'class' = c('Drift', 'logical'), is.drifting = !is.na(timestamp),
            mu = mu, sigma = sigma, year.seasonality = year.seasonality,
            timestamp = timestamp, rate = rate, significance = significance)
}


#' @rdname Drift
#'
Drift.Arima <- function(model, timestamps, ...) {

  mu <- model$coef[['intercept']]
  sigma <- sqrt(model$sigma2)
  ys <- setNames(model$coef[c('sin(31557600)', 'cos(31557600)')], c('sine', 'cosine'))
  x <- rep(FALSE, length(timestamps))

  Drift(x, mu = mu, sigma = sigma,
        year.seasonality = ys)
}


#' @rdname Drift
#'
Drift.ArimaExt <- function(model, timestamps, sig.treshold) {

  stopifnot(!is.null(model$xreg))
  stopifnot('bptrend' %in% colnames(model$xreg))

  sig <- model$drift.significance
  x <- rep(FALSE, length(timestamps))

  if (sig > sig.treshold) {
    mu <- model$MND$coef[['intercept']]
    sigma <- sqrt(model$MND$sigma2)
    ys <- setNames(model$MND$coef[c('sin(31557600)', 'cos(31557600)')], c('sine', 'cosine'))
    return(Drift(x, mu = mu, sigma = sigma, year.seasonality = ys))
  } else {
    mu <- model$coef[['intercept']]
    sigma <- sqrt(model$sigma2)
    ys <- setNames(model$coef[c('sin(31557600)', 'cos(31557600)')], c('sine', 'cosine'))
  }

  x[timestamps >= model$bp.ts] <- TRUE

  Drift(x, mu = mu, sigma = sigma,
        timestamp = model$bp.ts,
        significance = sig,
        year.seasonality = ys,
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
#' @param significance level at which to detect drift. Defaults to 0.05.
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
           ..., plot = FALSE, verbose = FALSE, title = NULL, significance = 0.05) {

    if (is.null(timestamps)) stop('Drift detection requires a timestamp for each observation x.')
    if (length(timestamps) != length(x)) stop('x and timestamps must have same length.')

    assert.timestamp(timestamps)
    assert.nonas(timestamps)
    #assert.noduplicates(timestamps) # ToDo: decide to enable or disable.
    assert.ordered(timestamps)

    assert.nonas(x)

    if (apriori$data_type != "air pressure" && apriori$units != 'cmH2O')
      stop('Drift detection is only implemented for air pressure data in cmH2O units.')

    standardGeneric('detect_drift')
  }
)


#' Drift detection function
#' @importFrom methods setMethod
#' @describeIn detect_drift
#'
setMethod(
  'detect_drift',
  signature = c(x = "numeric"),
  function(x, timestamps, reference, apriori, plot, verbose, title, significance) {

    # make differences of x with the reference in respect to matching timestamps: dr$x = x - referece
    dr <- drift_reference.differentiate(x = x, timestamps = timestamps, reference = reference)

    # aggregate dr for model fitting
    dra <- dr[, .(x = median(x)), by = .(timestamps)]

    if (nrow(dra) >= 2L) {
      # fit the drift model
      model <- model_drifts.fit(dr.x = dra$x, dr.ts = dra$timestamps, ar1 = 0.9, dfdiff = 2.8)
      # convert model to Drift object which is then returned to the user
      drift <- Drift(model, timestamps, sig.treshold = significance)
    } else {
      warning('x and reference data have no mathcing timestamps. ' %||%
              'At least 2 matches are required for computing drift.',
              call. = FALSE, immediate. = TRUE)
      drift <- Drift(rep(FALSE, length(x)), mu = NA)
    }

    if (plot) plot_drifts(x = x, timestamps = timestamps, dr = dr, drift = drift, title = title)

    if (verbose) drift else as.vector(drift)
})
