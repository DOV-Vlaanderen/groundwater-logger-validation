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

drift_reference.differentiate <- function(x, timestamps, reference) {

  drift_reference.assert(x = x, timestamps = timestamps, reference = reference)

  df <- data.table::data.table(x, timestamps)
  df <- df[, .(x = mean(x)), by = .(timestamps = round.timestamp(timestamps, scalefactor.sec = 3600*12))]
  data.table::setkey(df, 'timestamps')

  df.ref <- data.table::rbindlist(reference, use.names = TRUE, fill = TRUE, idcol = 'reference')
  df.ref <- df.ref[!is.na(timestamps), .(x = mean(x, na.rm = TRUE)), by = .(reference, timestamps = round.timestamp(timestamps, scalefactor.sec = 3600*12))]
  data.table::setkey(df.ref, 'timestamps')

  df.diff <- df[J(df.ref), .('x' = x.x - i.x, timestamps, reference), nomatch = NULL][!is.na(x), .(x = median(x)), by = .(timestamps)]
  data.table::setkey(df.diff, 'timestamps')

  df.diff
}
