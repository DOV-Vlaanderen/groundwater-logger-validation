#' @keywords internal
#'
drift_reference.assert <- function(reference) {
  if (!is.list(reference) || length(reference) == 0L)
    stop('Current version of drift detection requires reference barometer data. ' %||%
           'See ?gwloggeR::detect_drift for how to supply it.', call. = FALSE)

  lapply(length(reference), function(id) {

    if (!all(c('x', 'timestamps') %in% names(reference[[id]])))
      stop('Reference data should be supplied as list of lists. ' %||%
             'E.g. list(list(x, timestamps), list(x, timestamps), ...)', call. = FALSE)

    assert.timestamp(reference[[id]][['timestamps']])
    assert.nonas(reference[[id]][['x']])
    assert.numeric(reference[[id]][['x']])

  })
}


#' Aggregate df to specified scalefactor
#'
#' @keywords internal
#'
drift_reference.aggregate <- function(df, scalefactor.sec, is.reference = FALSE) {
  stopifnot(is.list(df) || is.data.frame(df))

  df <- data.table::as.data.table(df) # in case df is a reference (i.e. list)
  data.table::setkey(df, 'timestamps')

  df.new <- df[, .('x' = mean(x)), by = .('timestamps' = round.timestamp(timestamps, scalefactor.sec = scalefactor.sec))]
  data.table::setkey(df.new, 'timestamps')

  # If x has intervals smaller than scalefactor.sec, then it is also not a
  # problem since the aggregated will be used.

  # Reference data should have intervals smaller or equal to scalefactor.sec
  # If reference data has intervals larger than scalefactor.sec,
  # then it will not be possible to calculate the difference with original
  # series for these observations.

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
  df.x <- drift_reference.aggregate(data.frame(x, timestamps), scalefactor.sec = scalefactor.sec)
  data.table::setkey(df.x, 'timestamps')

  df.ref <- data.table::rbindlist(
    lapply(reference, drift_reference.aggregate, scalefactor.sec = scalefactor.sec, is.reference = TRUE),
    use.names = TRUE, fill = TRUE, idcol = 'reference.id')
  data.table::setkey(df.ref, 'timestamps')

  df.diff <- df.ref[J(df.x), .('x' = i.x - x.x, timestamps, reference.id, reference.x = x.x)]

  # Missing reference (mr) data
  mr.prop <- sum(is.na(df.diff$x))/nrow(df.diff)
  if (mr.prop > 0.001)
    warning(sprintf('There is for %.2f %% of barometer observations not a single reference data.', mr.prop*100),
            call. = FALSE, immediate. = TRUE)

  df.diff <- df.diff[!is.na(x), ] # remove observations that have no reference data
  data.table::setkey(df.diff, 'timestamps')

  # Left side
  if (df.x[1L, timestamps] < df.ref[1L, timestamps])
    stop(sprintf('Reference data starts at %s while the barometer data starts sooner: %s. ' %||%
                   'Either provide the barometer data only from %1$s, or add reference data down till %s.',
                 min(df.ref$timestamps), min(timestamps), df.x[1L, timestamps]), call. = FALSE)

  # Right side
  if (df.x[.N, timestamps] > df.ref[.N, timestamps])
    stop(sprintf('Reference data ends at %s while the barometer data ends later: %s. ' %||%
                   'Either provide the barometer data only till %1$s, or add reference data up till %s.',
                 max(df.ref$timestamps), max(timestamps), df.x[.N, timestamps]), call. = FALSE)

  df.diff
}
