#' @keywords internal
histogram <- function(x, outliers) {
  # Freedman–Diaconis rule
  # https://en.wikipedia.org/wiki/Freedman%E2%80%93Diaconis_rule
  # The multiplier is adjusted to allow for slightly more bins.
  multiplier <- 1.5
  n <- length(x)
  binwidth <- multiplier * IQR(x) / n ^ (1/3)
  fun.density <- attr(outliers, "fun.density")
  ggplot2::ggplot(data = data.frame(x), mapping = ggplot2::aes_string(x = 'x')) +
    ggplot2::geom_histogram(binwidth = binwidth, fill = 'black') +
    ggplot2::stat_function(fun =  function(x) fun.density(x) * n * binwidth,
                  color = "green", geom = 'density', fill = 'green', alpha = 0.2) +
    ggplot2::geom_vline(xintercept = attr(outliers, "cutpoints"), color = 'red') +
    ggplot2::ylab('frequency') +
    ggplot2::theme_light()
}

#' @keywords internal
qqplot <- function(x, outliers) {
  probs <- c(0.25, 0.75)
  fit <- lm(quantile(x, probs, names = FALSE, na.rm = TRUE) ~ qnorm(probs))
  ggplot2::ggplot(data = data.frame('sample' = x, 'theoretical' = qqnorm(x, plot.it = FALSE)$x, outliers)) +
    ggplot2::geom_abline(intercept = coef(fit)[1], slope = coef(fit)[2]) + # inspired by qqline()
    ggplot2::geom_point(mapping = ggplot2::aes_string(x = "theoretical", y = "sample", color = "outliers"), show.legend = FALSE) +
    ggplot2::scale_color_manual(name = "OUTLIER", values = c("FALSE" = "black", "TRUE" = "red")) +
    ggplot2::geom_hline(yintercept = attr(outliers, 'cutpoints'), color = 'red') +
    ggplot2::ylab('x') + ggplot2::xlab('theoretical quantiles') +
    ggplot2::theme_light()
}

#' @keywords internal
scatterplot <- function(x, outliers, timestamps = NULL) {
  n <- length(x)
  ggplot2::ggplot(data = data.frame(x = if (is.null(timestamps)) 1:n else timestamps,
                                    y = x, outliers), mapping = ggplot2::aes_string(x = "x", y = "y")) +
    ggplot2::geom_line() +
    ggplot2::geom_point(mapping = ggplot2::aes_string(color = "outliers"), show.legend = FALSE) +
    ggplot2::scale_color_manual(name = "OUTLIER", values = c("FALSE" = "black", "TRUE" = "red")) +
    ggplot2::geom_hline(yintercept = attr(outliers, 'cutpoints'), color = 'red') +
    ggplot2::ylab('x') + ggplot2::xlab(if (is.null(timestamps)) 'sequence' else 'timestamp') +
    ggplot2::theme_light()
}

#' @keywords internal
outliers_plot <- function(x, outliers, timestamps = NULL, show.qqplot = TRUE, title) {
  h <- histogram(x, outliers = outliers)
  q <- if (show.qqplot) qqplot(x, outliers) else grid::grob()
  s <- scatterplot(x, outliers = outliers, timestamps = timestamps)
  layout_matrix <- rbind(c(1,2),
                         c(3,3))
  grob.title <- if (!is.null(title)) grid::textGrob(title, x = 0.05, hjust = 0)
  gridExtra::grid.arrange(h, q, s, layout_matrix = layout_matrix,
                          top = grob.title)
}

#set.seed(2019)
#print(histogram(rnorm(10000), function(x) dnorm(x, 0, 1), c(-3.5, 3.5)))
#print(qqplot(rnorm(10000), c(TRUE, rep(FALSE, 9999)), c(-3.5, 3.5)))
#print(scatterplot(rnorm(10000), FALSE, c(-3.5, 3.5)))
#outlierplot(rnorm(10000), FALSE, c(-3.5, 3.5), 0, 1)
