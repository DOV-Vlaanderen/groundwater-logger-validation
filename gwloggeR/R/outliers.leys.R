#' @title Detects outliers
#' @description
#' This function is based on Leys, C. e.a., Detecting outliers, 2013.
#'
detect_outliers_leys <- function(x, p.value = 0.0005, verbose = FALSE, M = mean(x), MAD = mad(x)) {
  if (is.na(MAD) | MAD == 0) {
    return(rep(FALSE, length(x)))
  }
  # sigma.reject is the sigma after which we reject points. The 0.0005 means that
  # in 1 of 5000 calculations, we will reject a value we shouldn't have.
  sigma.reject <- qnorm(p = (1 - p.value)^(1 / length(x)))
  x.rejects <- abs((x - M) / MAD) > sigma.reject
  x.rejects[which(is.na(x.rejects))] <- FALSE # NA's are no outliers
  if (verbose) {
    print(sprintf(
      "M: %.3f | MAD: %.3f | sigma.reject: %.3f | # rejects: %i | sd: %.3f | sd.clean: %.3f",
      M, MAD, sigma.reject, sum(x.rejects, na.rm = TRUE),
      sqrt(var(x, na.rm = TRUE)), sqrt(var(x[!x.rejects], na.rm = TRUE))
    ))
  }
  x.rejects
}
