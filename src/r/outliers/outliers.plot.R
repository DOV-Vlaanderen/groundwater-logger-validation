plot.outliers <- function(x = NULL, y, outliers, title = NULL) {
  require(ggplot2)

  stopifnot(is.logical(outliers))

  df <- data.frame("x" = if (!is.null(x)) x else 1L:length(y),
                   "y" = y,
                   "outliers" = outliers)

  title <- ggtitle(title, subtitle = with(df, {
    freq <- table(outliers)
    notime <- sum(is.na(x))
    paste(c(names(freq), 'NOTIME'), c(freq, notime), sep = '=', collapse = ', ')
  }))

  ggplot(data = df, mapping = aes(x = x, y = y)) +
    geom_line() +
    geom_point(mapping = aes(color = outliers), show.legend = TRUE) +
    scale_color_manual(name = "OUTLIER", values = c("FALSE" = "black", "TRUE" = "red")) +
    title +
    theme(legend.position="bottom", legend.justification = 1) +
    theme(axis.title.x=element_blank()) +
    ylab(deparse(substitute(y)))
}
