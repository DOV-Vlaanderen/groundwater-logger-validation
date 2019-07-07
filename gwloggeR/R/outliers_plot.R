#' @keywords internal
histogram <- function(x, mean, sd, cutpoints) {
  # Freedman–Diaconis rule
  # https://en.wikipedia.org/wiki/Freedman%E2%80%93Diaconis_rule
  # The multiplier is adjusted to allow for slightly more bins.
  multiplier <- 1.5
  n <- length(x)
  binwidth <- multiplier * IQR(x) / n ^ (1/3)
  ggplot2::ggplot(data = data.frame(x), mapping = ggplot2::aes(x = x)) +
    ggplot2::geom_histogram(binwidth = binwidth, fill = 'black') +
    ggplot2::stat_function(fun = function(x) dnorm(x, mean = mean, sd = sd) * n * binwidth,
                  color = "green", geom = 'density', fill = 'green', alpha = 0.2) +
    ggplot2::geom_vline(xintercept = cutpoints, color = 'red') +
    ggplot2::ylab('frequency') +
    ggplot2::theme_light()
}

#' @keywords internal
qqplot <- function(x, outliers, cutpoints) {
  probs <- c(0.25, 0.75)
  fit <- lm(quantile(x, probs, names = FALSE, na.rm = TRUE) ~ qnorm(probs))
  ggplot2::ggplot(data = data.frame('sample' = x, 'theoretical' = qqnorm(x, plot.it = FALSE)$x, outliers)) +
    ggplot2::geom_abline(intercept = coef(fit)[1], slope = coef(fit)[2]) + # inspired by qqline()
    ggplot2::geom_point(mapping = ggplot2::aes(x = theoretical, y = sample, color = outliers), show.legend = FALSE) +
    ggplot2::scale_color_manual(name = "OUTLIER", values = c("FALSE" = "black", "TRUE" = "red")) +
    ggplot2::geom_hline(yintercept = cutpoints, color = 'red') +
    ggplot2::ylab('x') + ggplot2::xlab('theoretical quantiles') +
    ggplot2::theme_light()
}

#' @keywords internal
scatterplot <- function(x, outliers, cutpoints) {
  n <- length(x)
  ggplot2::ggplot(data = data.frame(x = 1:n, y = x, outliers), mapping = ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_line() +
    ggplot2::geom_point(mapping = ggplot2::aes(color = outliers), show.legend = FALSE) +
    ggplot2::scale_color_manual(name = "OUTLIER", values = c("FALSE" = "black", "TRUE" = "red")) +
    ggplot2::geom_hline(yintercept = cutpoints, color = 'red') +
    ggplot2::ylab('x') + ggplot2::xlab('sequence') +
    ggplot2::theme_light()
}

#' @keywords internal
outlierplot <- function(x, outliers, cutpoints, mean, sd) {
  h <- histogram(x, mean = mean, sd = sd, cutpoints = cutpoints)
  q <- qqplot(x, outliers, cutpoints)
  s <- scatterplot(x, outliers = outliers, cutpoints = cutpoints)
  layout_matrix <- rbind(c(1,2),
                         c(3,3))
  gridExtra::grid.arrange(h, q, s, layout_matrix = layout_matrix)
}

#set.seed(2019)
#print(histogram(rnorm(10000), 0, 1, c(-3.5, 3.5)))
#print(qqplot(rnorm(10000), c(TRUE, rep(FALSE, 9999)), c(-3.5, 3.5)))
#print(scatterplot(rnorm(10000), FALSE, c(-3.5, 3.5)))
#outlierplot(rnorm(10000), FALSE, c(-3.5, 3.5), 0, 1)
