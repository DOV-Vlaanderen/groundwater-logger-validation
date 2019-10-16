#' @keywords internal
differenceplot <- function(x, timestamps = NULL, outliers = FALSE) {
  n <- length(x)
  timestamps.invalid <- is.null(timestamps) | all(is.na(timestamps))
  x.axis <- if (timestamps.invalid) 1:n else timestamps
  data <- data.frame(x = x.axis, y = x, outliers)[order(x.axis),]
  data <- data[!is.na(data[['x']]),]
  data[['y.diff']] <- c(NA, diff(data[['y']]))

  ggplot2::ggplot(data = data, mapping = ggplot2::aes_string(x = "x", y = "y.diff")) +
    ggplot2::geom_line(na.rm = TRUE) +
    ggplot2::geom_point(mapping = ggplot2::aes_string(color = "outliers"), show.legend = FALSE, na.rm = TRUE) +
    ggplot2::scale_color_manual(name = "OUTLIER", values = c("FALSE" = "black", "TRUE" = "red")) +
    ggplot2::ylab('difference(x)') + ggplot2::xlab(if (timestamps.invalid) 'sequence' else 'timestamp') +
    ggplot2::theme_light()
}
