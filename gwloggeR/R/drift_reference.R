#' @keywords internal
#'
drift_reference.assert <- function(x, timestamps, reference) {
  if (!is.list(reference) || length(reference) == 0L)
    stop('Current version of drift detection requires reference barometer data. ' %||%
           'See ?gwloggeR::detect_drift for how to supply it.', call. = FALSE)

  if ('x' %in% names(reference) || 'timestamps' %in% names(reference))
    stop('Reference data should be supplied as list of lists. ' %||%
           'E.g. list(list(x, timestamps), list(x, timestamps), ...)', call. = FALSE)

  if (length(reference) > 1L)
    stop('Current version of drift detection requires exactly 1 reference barometer.', call. = FALSE)

  # TODO: check that enough reference data is supplied...
}


#' Aggregate df to specified scalefactor
#'
#' @keywords internal
#'
drift_reference.aggregate <- function(df, scalefactor.sec) {
  df <- data.table::as.data.table(df) # in case df is a reference (i.e. list)
  df.new <- df[, .(x = mean(x)), by = .(timestamps = round.timestamp(timestamps, scalefactor.sec = scalefactor.sec))]
  data.table::setkey(df.new, 'timestamps')

  if (nrow(df) != nrow(df.new))
    warning(sprintf('Timeseries has intervals smaller than %ih. ', scalefactor.sec/3600) %||%
            sprintf('Measurements are averaged to %ih intervals.', scalefactor.sec/3600),
            call. = FALSE, immediate. = TRUE)

  df.new
}


#' Takes the differences between x and the reference values
#'
#' @return A data.frame (x, timestamps, reference.id) with the differences.
#' Note that in case of more than 1 reference is supplied, there might be
#' duplicate timestamps based on different reference barometers.
#'
#' @keywords internal
#'
drift_reference.differentiate <- function(x, timestamps, reference, scalefactor.sec) {

  # Aggregating x to minimum 12h intervals so differences can be taken on specific timestamps
  drift_reference.assert(x = x, timestamps = timestamps, reference = reference)

  df <- drift_reference.aggregate(data.frame(x, timestamps), scalefactor.sec = scalefactor.sec)
  data.table::setkey(df, 'timestamps')

  df.ref <- data.table::rbindlist(
    lapply(reference, drift_reference.aggregate, scalefactor.sec = scalefactor.sec),
    use.names = TRUE, fill = TRUE, idcol = 'reference.id')
  data.table::setkey(df.ref, 'timestamps')

  df.diff <- df[J(df.ref), .('x' = x.x - i.x, timestamps, reference.id, reference.x = i.x), nomatch = NULL][!is.na(x), ]
  data.table::setkey(df.diff, 'timestamps')

  df.diff
}
