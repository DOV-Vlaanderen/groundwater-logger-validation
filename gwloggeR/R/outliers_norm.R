#' @title Detects outliers
#' @description ...
#' @keywords internal
detect_outliers_norm <- function(x, p.value = 0.0005, verbose = FALSE, x.mean, x.sd,
                                 type = c("two.sided", "less", "greater")) {
  type <- match.arg(type)

  if (is.na(x.sd) | x.sd == 0) {
    return(rep(FALSE, length(x)))
  }

  # sigma.reject is the sigma after which we reject points. The 0.0005 means that
  # in 1 of 2000 (1/0.0005) calculations, we will reject a value we shouldn't have.
  sigma.reject <- qnorm(p = (1 - p.value)^(1 / length(x)))
  x.norm <- (x - x.mean) / x.sd
  x.rejects <- switch (type,
    "two.sided" = abs(x.norm) > sigma.reject,
    "greater" = x.norm > sigma.reject,
    "less" = x.norm < -sigma.reject
  )
  x.rejects[which(is.na(x.rejects))] <- FALSE # NA's are no outliers

  if (verbose) {
    print(sprintf(
      "M: %.3f | SD: %.3f | sigma.reject: %.3f | # rejects: %i | sd: %.3f | sd.clean: %.3f",
      x.mean, x.sd, sigma.reject, sum(x.rejects, na.rm = TRUE),
      sqrt(var(x, na.rm = TRUE)), sqrt(var(x[!x.rejects], na.rm = TRUE))
    ))
  }

  x.rejects
}
