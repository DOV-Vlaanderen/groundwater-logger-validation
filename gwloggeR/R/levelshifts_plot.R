#' @keywords internal
scatterplot.levelshifts <- function(x, levelshifts = rep(FALSE, length(x)), timestamps = NULL) {
  n <- length(x)
  timestamps.invalid <- is.null(timestamps) | all(is.na(timestamps))
  x.axis <- if (timestamps.invalid) 1:n else timestamps
  data <- data.frame(x = x.axis, y = x, levelshifts)[order(x.axis),]

  start.idx <- unique(c(1L, which(data$levelshifts)))
  end.idx <- unique(c(start.idx[-1L], length(x)))
  col <- c('green', ifelse(data$y[start.idx[-1L] -1L] < data$y[start.idx[-1L]], 'red', 'blue'))

  ggplot2::ggplot(data = data, mapping = ggplot2::aes_string(x = "x", y = "y")) +
    ggplot2::geom_line() +
    ggplot2::geom_point(data = data[c(start.idx[-1L], start.idx[-1L]-1L),]) +
    ggplot2::annotate("rect", xmin = data$x[start.idx], xmax = data$x[end.idx],
                      ymin = -Inf, ymax = Inf, alpha = 0.1, fill = col) +
    ggplot2::geom_vline(xintercept = x.axis[start.idx[-1L]], linetype = 'dashed') +
    ggplot2::scale_color_manual(name = "OUTLIER", values = c("FALSE" = "black", "TRUE" = "red")) +
    ggplot2::ylab('x') + ggplot2::xlab(if (timestamps.invalid) 'sequence' else 'timestamp') +
    ggplot2::theme_light()
}

differenceplot <- function(x, timestamps = NULL) {
  xdiff <- c(NA, diff(x))
  n <- length(x)
  timestamps.invalid <- is.null(timestamps) | all(is.na(timestamps))
  x.axis <- if (timestamps.invalid) 1:n else timestamps
  data <- data.frame(x = x.axis, y = xdiff, outliers = FALSE)[order(x.axis),]

  ggplot2::ggplot(data = data, mapping = ggplot2::aes_string(x = "x", y = "y")) +
    ggplot2::geom_line(na.rm = TRUE) +
    ggplot2::geom_point(mapping = ggplot2::aes_string(color = "outliers"), show.legend = FALSE, na.rm = TRUE) +
    ggplot2::scale_color_manual(name = "OUTLIER", values = c("FALSE" = "black", "TRUE" = "red")) +
    ggplot2::ylab('x') + ggplot2::xlab(if (timestamps.invalid) 'sequence' else 'timestamp') +
    ggplot2::theme_light()
}

#set.seed(2020)
#print(scatterplot.levelshifts(rnorm(100), rep(c(FALSE, TRUE, FALSE, TRUE, FALSE), c(10, 1, 59, 1, 29)),
#      seq(Sys.time(), Sys.time() + 10, length.out = 100)))
#print(scatterplot.levelshifts(rnorm(100), rep(c(FALSE, TRUE, FALSE, TRUE, FALSE), c(10, 1, 59, 1, 29))))
#print(scatterplot.levelshifts(rnorm(100)))
#print(differenceplot(rnorm(100), timestamps = seq(Sys.time(), Sys.time() + 24*60*60, length.out = 100)))
