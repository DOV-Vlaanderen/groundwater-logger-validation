#' @title Base functions for plotting hydrostatic pressure data
#' @keywords internal
#'
plot.base <- function(data) {
    if (!identical(1:nrow(data), order(data$x))) stop('ERROR: data must be ordered.')

  ggplot2::ggplot(data = data, mapping = ggplot2::aes_string(x = "x", y = "y")) +
    ggplot2::geom_line(alpha = .4) +
    ggplot2::ylab('x') + ggplot2::xlab(if (is.timestamp(data$x)) 'timestamp' else 'sequence') +
    ggplot2::theme_light()
}


#' @keywords internal
#'
plot.add.levelshifts <- function(plot) {
  data <- plot$data
  idx.start <- unique(c(1L, which(data$levelshifts)))
  idx.end <- c(idx.start[-1L], nrow(data))
  col <- c('green', ifelse(data[idx.start[-1L] - 1L, y] < data[idx.start[-1L], y], 'red', 'blue'))
  idx.black <- setdiff(idx.start[-1L]-1L, idx.start[-1L])

  plot +
    ggplot2::geom_point(data = data[idx.black,]) +
    ggplot2::geom_point(data = data[idx.start[-1L],], color = 'red') +
    ggplot2::annotate("rect", xmin = data[idx.start, x], xmax = data[idx.end, x],
                      ymin = -Inf, ymax = Inf, alpha = 0.1, fill = col) +
    # Old ggplot geom_vline doesn't support posixct x, hence as.numeric(x)
    ggplot2::geom_vline(xintercept = data[idx.start[-1L], as.numeric(x)], linetype = 'dashed')
}


#' @keywords internal
#'
plot.add.outliers <- function(plot) {
  data <- plot$data
  idx <- which(data$outliers)
  idx <- as.vector(rbind(idx, idx, idx) + -1:1)
  idx <- idx[idx <= nrow(data)]

  plot +
    ggplot2::geom_point(data = data[idx, ],
                        mapping = ggplot2::aes_string(color = "outliers"),
                        show.legend = FALSE) +
    ggplot2::scale_color_manual(name = "OUTLIER",
                                values = c("FALSE" = "black", "TRUE" = "red"))
}


#' @keywords internal
#'
plot.add.tempchanges <- function(plot) {
  data <- data.table::copy(plot$data)
  idx.red <- which(data$tempchanges)
  idx.black <- setdiff(unique(pmax(idx.red - 1L, 1L)), idx.red)

  data[c(idx.red, idx.black), group := TRUE]
  data[, group := data.table::rleid(group)]

  plot +
    ggplot2::geom_line(data = data[c(idx.red, idx.black),],
                       mapping = ggplot2::aes(group = group),
                       color = 'red') +
    ggplot2::geom_point(data = data[idx.red,], color = 'red') +
    ggplot2::geom_point(data = data[idx.black,], color = 'black')
}


#' @keywords internal
#'
plot.data <- function(x, timestamps = NULL, events) {
  n <- length(x)
  timestamps.invalid <- is.null(timestamps) | all(is.na(timestamps))
  x.axis <- if (timestamps.invalid) 1:n else timestamps
  data <- data.table::data.table(x = x.axis, y = x,
                                 outliers = Outliers(events),
                                 levelshifts = Levelshifts(events),
                                 tempchanges = Temporalchanges(events))
  data <- data[!is.na(x),]
  data.table::setkey(data, x)

  data
}


#' @keywords internal
#'
plot.generic <- function(x, timestamps, events, title) {
  df.plot <- plot.data(x = x, timestamps = timestamps, events = events)
  g <- plot.base(data = df.plot)
  g <- plot.add.levelshifts(g)
  g <- plot.add.tempchanges(g)
  g <- plot.add.outliers(g)
  g <- g + ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                          axis.text.x = ggplot2::element_blank(),
                          axis.text.y = ggplot2::element_text(angle = 90, hjust = 0.5))

  d <- differenceplot(x = x, timestamps = timestamps, outliers = df.plot[J(timestamps), outliers | levelshifts | tempchanges]) +
    ggplot2::theme(axis.text.y = ggplot2::element_text(angle = 90, hjust = 0.5))

  layout_matrix <- rbind(c(1),
                         c(2))
  grob.title <- if (!is.null(title)) grid::textGrob(title, x = 0.05, hjust = 0)
  gridExtra::grid.arrange(g, d, layout_matrix = layout_matrix,
                          top = grob.title)
}
