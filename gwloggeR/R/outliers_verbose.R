#' @keywords internal
outliers_verbose <- function(outliers) {
  sprintf(paste0(
    "Robust mu estimate based on median: %0.3f\n",
    "Robust sigma estimate based on MAD: %0.3f\n",
    "Calculated standardized sigma cutoff (based on alpha = %s): %0.3f\n",
    "Calculated cutoff: (%0.3f, %0.3f)\n")
    , attr(outliers, 'x.mean')
    , attr(outliers, 'x.sd')
    , attr(outliers, 'alpha'), attr(outliers, 'sigma.reject')
    , attr(outliers, 'cutpoints')[1L], attr(outliers, 'cutpoints')[2L])
}

