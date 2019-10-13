#' @keywords internal
#'
plot.base <- function(data) {
    if (!identical(1:nrow(data), order(data$x))) stop('ERROR: data must be ordered.')

  ggplot2::ggplot(data = data, mapping = ggplot2::aes_string(x = "x", y = "y")) +
    ggplot2::geom_line(alpha = .4) +
    ggplot2::ylab('x') + ggplot2::xlab(if (is.timestamp(data$x)) 'sequence' else 'timestamp') +
    ggplot2::theme_light()
}

plot.add.levelshifts <- function(plot) {
  data <- plot$data
  start.idx <- unique(c(1L, which(data$levelshifts)))
  end.idx <- c(start.idx[-1L], nrow(data))
  col <- c('green', ifelse(data[start.idx[-1L] - 1L, y] < data[start.idx[-1L], y], 'red', 'blue'))
  idx <- unique(c(start.idx[-1L], start.idx[-1L]-1L))

  plot +
    ggplot2::geom_point(data = data[idx,]) +
    ggplot2::annotate("rect", xmin = data[start.idx, x], xmax = data[end.idx, x],
                      ymin = -Inf, ymax = Inf, alpha = 0.1, fill = col) +
    ggplot2::geom_vline(xintercept = data[start.idx[-1L], x], linetype = 'dashed')
}

plot.add.outliers <- function(plot) {
  data <- plot$data
  idx <- which(data$outliers)
  idx <- as.vector(rbind(idx, idx, idx) + -1:1)
  idx <- idx[idx <= nrow(data)]

  plot +
    ggplot2::geom_point(data = data[idx, ],
                        mapping = ggplot2::aes_string(color = "outliers"), show.legend = FALSE) +
    ggplot2::scale_color_manual(name = "OUTLIER", values = c("FALSE" = "black", "TRUE" = "red"))
}

plot.add.tempchanges <- function(plot) {
  data <- data.table::copy(plot$data)
  idx <- which(data$tempchanges)
  idx <- sort(unique(pmax(c(idx, idx - 1L), 1L)))

  data[idx, group := TRUE]
  data[, group := data.table::rleid(group)]

  plot +
    ggplot2::geom_point(data = data[idx,], color = 'blue') +
    ggplot2::geom_line(data = data[idx,], mapping = ggplot2::aes(group = group),
                       color = 'red', alpha = .6)
}

# plot.data <- function(x, timestamps = NULL, outliers = NULL, levelshifts = NULL) {
#   n <- length(x)
#   timestamps.invalid <- is.null(timestamps) | all(is.na(timestamps))
#   x.axis <- if (timestamps.invalid) 1:n else timestamps
#   data <- data.frame(x = x.axis, y = x, levelshifts, outliers)[order(x.axis),]
#   data <- data[!is.na(data$x),]
#   data
# }

plot.data <- function(x, timestamps = NULL, events) {
  n <- length(x)
  timestamps.invalid <- is.null(timestamps) | all(is.na(timestamps))
  x.axis <- if (timestamps.invalid) 1:n else timestamps
  data <- data.table::data.table(x = x.axis, y = x, outliers = FALSE, levelshifts = FALSE,
                                 tempchanges = FALSE)
  data[events[type == 'AO', index], outliers := TRUE]
  data[events[type == 'LS', index], levelshifts := TRUE]

  df.decay <- events[type == 'TC', .('decay' = alpha*delta^(0:1000)), by = index][abs(decay) > 5, .(decay, .N), by = index]
  if (nrow(df.decay) > 0L) data[df.decay[, .('idx' =index:(index + N - 1L)), by = .(index, N)][, idx], tempchanges := TRUE]

  data <- data[!is.na(x),]
  data.table::setkey(data, x)

  data
}

plot.generic <- function(x, timestamps, events, title) {
  df.plot <- plot.data(x = x, timestamps = timestamps, events = events)
  g <- plot.base(data = df.plot)
  g <- plot.add.levelshifts(g)
  g <- plot.add.tempchanges(g)
  g <- plot.add.outliers(g)

  d <- differenceplot(x = x, timestamps = timestamps, outliers = df.plot[J(timestamps), outliers | levelshifts | tempchanges])

  layout_matrix <- rbind(c(1),
                         c(2))
  grob.title <- if (!is.null(title)) grid::textGrob(title, x = 0.05, hjust = 0)
  gridExtra::grid.arrange(g, d, layout_matrix = layout_matrix,
                          top = grob.title)
}



#' df <- plot.data('timestamps' = c(seq(Sys.time(), Sys.time() - 60*60*24*7, length.out = 99), NA), # wrong timestamp order + NA
#'                 'x' = rnorm(100),
#'                 'events' = data.table::data.table('type' = c('AO', 'LS', 'AO'), 'index' = c(10, 20, 71))
#'                 #'outliers' = rep(c(FALSE, TRUE, FALSE, TRUE, FALSE), c(9, 1, 60, 1, 29)),
#'                 #'levelshifts' = rep(c(FALSE, TRUE, FALSE), c(20, 1, 79))
#'                 )
#' df
#' tmp <- plot.base(df)
#' print(tmp)
#' tmp <- plot.add.levelshifts(tmp)
#' print(tmp)
#' tmp <- plot.add.outliers(tmp)
#' print(tmp)
