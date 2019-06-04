plot.outliers <- function(x = NULL, y, outliers, title = NULL) {
  require(ggplot2)

  stopifnot(is.logical(outliers))

  df <- data.frame("x" = if (all(is.na(x))) 1L:length(y) else x,
                   "y" = y,
                   "outliers" = outliers)

  freq <- table(outliers)
  notime <- sum(is.na(x))
  title <- ggtitle(title, subtitle = paste(c(names(freq), 'NOTIME'),
                                           c(freq, notime),
                                           sep = '=', collapse = ', '))

  ggplot(data = df, mapping = aes(x = x, y = y)) +
    geom_line() +
    geom_point(mapping = aes(color = outliers), show.legend = TRUE) +
    scale_color_manual(name = "OUTLIER", values = c("FALSE" = "black", "TRUE" = "red")) +
    title +
    theme(legend.position="bottom", legend.justification = 1) +
    theme(axis.title.x=element_blank()) +
    ylab(deparse(substitute(y)))
}

plot.dmst_cde <- function(x = NULL, y, dmst_cde, title = NULL) {
  require(ggplot2)

  df <- data.frame("x" = if (all(is.na(x))) 1L:length(y) else x,
                   "y" = y,
                   "dmst_cde" = dmst_cde)

  freq <- table(dmst_cde)
  dupes <- sum(duplicated(x, incomparables = NA))
  notime <- sum(is.na(x))
  title <- ggtitle(title,
                   subtitle = paste(c(names(freq), 'DUPES', 'NOTIME'),
                                    c(freq, dupes, notime),
                                    sep = '=', collapse = ', '))

  # in case of overlap, render according to this order:
  df <- df[order(factor(df$dmst_cde, levels = c("ENT","INV","DEL","VLD"))),]

  ggplot(data = df, mapping = aes(x = x, y = y)) +
    geom_line() +
    geom_point(mapping = aes(color = dmst_cde), show.legend = TRUE) +
    scale_color_manual(name = "DRME_DMST_CDE",
                       values = c("ENT" = "black", "INV" = "red",
                                  "DEL" = "magenta", "VLD" = "green")) +
    title +
    theme(legend.position="bottom", legend.justification = 1) +
    theme(axis.title.x=element_blank()) +
    ylab(deparse(substitute(y)))
}
