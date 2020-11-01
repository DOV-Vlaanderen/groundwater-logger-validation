#' @keywords internal
c.norm.optimal <- function(alpha, n, type = c("two.sided", "one.sided")) {
  type <- match.arg(type)

  TS <- if (type == "two.sided") 2 else 1

  -stats::qnorm((1-(1-alpha)^(1/n))/TS)
}


#' @title Detects outliers
#' @description Detect outliers based on normality assumption.
#' @keywords internal
detect_outliers_norm <- function(x, alpha = CONST.ALPHA, verbose = FALSE, x.mean, x.sd,
                                 type = c("two.sided", "less", "greater")) {
  type <- match.arg(type)
  N <- length(x)

  return.obj <- function(x.rejects, sigma.reject = NULL) {
    Outliers(x.rejects, x.mean = x.mean, x.sd = x.sd,
             sigma.reject = sigma.reject, alpha = alpha, type = type,
             fun.density = function(x) stats::dnorm(x, x.mean, x.sd),
             cutpoints = c(-1, 1) * sigma.reject * x.sd + x.mean)
  }

  if (is.na(x.sd) | x.sd == 0) {
    return(return.obj(rep(FALSE, N)))
  }

  # sigma.reject is the sigma after which we reject points. The 0.0005 means that
  # in 1 of 2000 calculations, we will reject a value we shouldn't have.
  sigma.reject <- c.norm.optimal(alpha = alpha, n = N,
                                 type = if (type == "two.sided") "two.sided" else "one.sided")
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

  return.obj(x.rejects, sigma.reject = sigma.reject)
}
